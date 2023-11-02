(ns gpu.wort.tools
  #?(:clj (:require [clojure.walk :refer [prewalk
                                          prewalk-replace]])))

#?(:clj
   (defmacro unquotable [& expressions]
     (let [quote-replacement (gensym 'kudzu_REPLACED_QUOTE)
           splice-keyword (keyword (gensym 'splice))]
       (letfn [(inline-unquotes
                 [form]
                 (let [replacement-map-atom (atom {})
                       inlined-replacements-form
                       (doall
                        (prewalk
                         (fn [subform]
                           (if (and (seq? subform)
                                    ('#{clojure.core/unquote
                                        clojure.core/unquote-splicing}
                                     (first subform)))
                             (let [replacement-binding (keyword (gensym))]
                               (swap! replacement-map-atom
                                      assoc
                                      replacement-binding
                                      (second subform))
                               (if (= (first subform)
                                      'clojure.core/unquote)
                                 replacement-binding
                                 (list splice-keyword replacement-binding)))
                             subform))
                         form))]
                   (list 'clojure.walk/prewalk
                         (prewalk-replace
                          {:splice-symbol splice-keyword}
                          '(fn [form]
                             (if (sequential? form)
                               (cond->
                                (reduce (fn [new-form subform]
                                          (if (and (list? subform)
                                                   (= (first subform)
                                                      :splice-symbol))
                                            (into new-form (second subform))
                                            (conj new-form subform)))
                                        []
                                        form)
                                 (not (vector? form)) seq)
                               form)))
                         (list 'clojure.walk/prewalk-replace
                               @replacement-map-atom
                               (list `quote
                                     (replace-quotes
                                      inlined-replacements-form))))))
               (replace-quotes
                 [form]
                 (if (and (seq? form)
                          (= (first form)
                             quote-replacement))
                   (let [subform (second form)]
                     (if (coll? subform)
                       (inline-unquotes subform)
                       (list `quote subform)))
                   form))]
         (->> expressions
              (prewalk-replace {`quote quote-replacement})
              (prewalk replace-quotes)
              (cons 'do))))))
