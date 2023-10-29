(ns gpu.wort.core
  (:require [gpu.util :as u]
            [clojure.string
             :refer [escape replace join]
             :rename {replace string-replace}]))

(defn indent [block]
  (str "  " (string-replace block "\n" "\n  ")))

(defn symbol->wgsl [s]
  (escape (str s)
          {"-" "_"}))

(def assignment-operators
  '#{= "/=" *= -= +=})

(def infix-operators
  '#{+ - * / % < <= > >= == != >> << & "^" | && ||})

(declare body->wgsl)

(defn form->wgsl [form]
  (cond
    (number? form) (str form)
    (boolean? form) (str form)
    (symbol? form) (symbol->wgsl form)

    (vector? form)
    (str (form->wgsl (first form))
         "["
         (form->wgsl (second form))
         "]")

    :else (let [[f & args] form]
            (cond

              (and (= f '-) (= (count args) 1))
              (str "-" (form->wgsl (first args)))

              ('#{++ --} f)
              (str (first args) f)

              ('#{let const var} f)
              (str f
                   " "
                   (symbol->wgsl (first args))
                   (when (= (count args) 3)
                     (str " : " (symbol->wgsl (second args)) " "))
                   " = "
                   (form->wgsl (last args)))

              (= '? f)
              (let [[condition true-case false-case] (map form->wgsl args)]
                (u/log [condition true-case false-case])
                (str "select("
                     true-case
                     ","
                     false-case
                     ","
                     condition
                     ")"))
              
              (= 'if f)
              (let [[condition true-block false-block] args
                    block-body (fn [block]
                                 (indent (body->wgsl
                                          (if (and (seq? block)
                                                   (= (first block) :block))
                                            (rest block)
                                            (list block)))))]
                (str "if "
                     (form->wgsl condition)
                     " {\n"
                     (block-body true-block)
                     "\n}"
                     (when false-block
                       (str "\nelse {\n"
                            (block-body false-block)
                            "\n}"))))
              
              (= 'when f)
              (str "if "
                   (form->wgsl (first args))
                   " {\n"
                   (indent (body->wgsl (rest args)))
                   "\n}")

              (= 'for f)
              (let [[initializer condition continuing]
                    (map form->wgsl (take 3 args))]
                (str "for("
                     initializer
                     "; "
                     condition
                     "; "
                     continuing
                     ") {\n"
                     (indent (body->wgsl (drop 3 args)))
                     "\n}"))

              (assignment-operators f)
              (join (str " " f " ") (map form->wgsl
                                         args))

              (infix-operators f)
              (str "("
                   (join (str " " f " ") (map form->wgsl
                                              args))
                   ")")

              :else (str f
                         "("
                         (join ", "
                               (map form->wgsl args))
                         ")")))))

(defn body->wgsl [body & [returns?]]
  (if (empty? body)
    ""
    (let [forms (map form->wgsl body)]
      (str (join "\n"
                 (map #(str % (when-not (= (last %) "}") ";"))
                      (if returns?
                        (concat (butlast forms)
                                (list (str "return " (last forms))))
                        forms)))))))

(defn function->wgsl [name fn-spec]
  (let [arg-list-index (some #(when (vector? (nth fn-spec %)) %) (range))
        tags (take arg-list-index fn-spec)
        argument-list (nth fn-spec arg-list-index)
        return (nth fn-spec (inc arg-list-index))
        body (drop (+ 2 arg-list-index) fn-spec)]
    (str
     (when (seq tags)
       (str (apply str (join " " (map (partial str "@") tags)))
            " "))
     "fn "
     (symbol->wgsl name)
     (if (empty? argument-list)
       "() "
       (str "(\n"
            (join
             ",\n"
             (map (fn [[argument-name argument-params]]
                    (let [argument-type (if (map? argument-params)
                                          (:type argument-params)
                                          argument-params)]
                      (indent
                       (str (when (map? argument-params)
                              (reduce #(str %1
                                            "@"
                                            (form->wgsl (symbol (first %2)))
                                            "("
                                            (form->wgsl (second %2))
                                            ") ")
                                      ""
                                      (dissoc argument-params :type)))
                            (symbol->wgsl argument-name)
                            " : "
                            argument-type))))
                  (partition 2 argument-list)))
            "\n) "))
     (when return
       (str "-> "
            (if (map? return)
              (str (reduce #(str %1 
                                 "@" 
                                 (form->wgsl (symbol(first %2))) 
                                 "("
                                 (form->wgsl (second %2))
                                 ") ")
                           ""
                           (dissoc return :type))
                   (symbol->wgsl (:type return)))
              return)
            " "))
     "{\n"
     (indent (body->wgsl body (not (nil? return))))
     "\n}")))

(defn functions->wgsl [functions]
  (apply str
         (apply concat
                (butlast
                 (interleave
                  (map (fn [[fn-name fn-spec]]
                         (str (function->wgsl fn-name fn-spec)
                              "\n"))
                       functions)
                  (repeat "\n"))))))

(defn uniforms->wgsl [uniforms]
  (join "\n"
        (mapcat (fn [uniform-group group-index]
                  (map (fn [[uniform-name uniform-type] binding-index]
                         (str "@group("
                              group-index
                              ") @binding("
                              binding-index
                              ") var<uniform> "
                              (symbol->wgsl uniform-name)
                              " : "
                              (symbol->wgsl uniform-type)
                              ";"))
                       (partition 2 uniform-group)
                       (range)))
                uniforms
                (range))))

(defn wort->wgsl [shader]
  (str (uniforms->wgsl (:uniforms shader))
       "\n\n"
       (functions->wgsl (:functions shader))))
