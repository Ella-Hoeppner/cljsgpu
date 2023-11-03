(ns gpu.wort.core
  (:require [gpu.util :as u]
            [clojure.walk :refer [prewalk]]
            [gpu.wort.macros :refer [default-macros]]
            [gpu.wort.compiler :refer [core-wort->wgsl]]))

(defn combine-chunks [& chunks]
  (let [merged-functions
        (apply (partial merge-with
                        (fn [body-1 body-2]
                          (vec (concat
                                (if (vector? body-1) body-1 (list body-1))
                                (if (vector? body-2) body-2 (list body-2))))))
               (map :functions chunks))
        merged-global (apply concat (map :global chunks))]
    (cond-> (apply (partial merge-with merge) chunks)
      merged-functions (assoc :functions merged-functions)
      (seq merged-global) (assoc :global merged-global))))

(defn apply-macros [{:keys [macros] :as shader} & [exclude-defaults?]]
  (let [chunks (atom nil)
        new-shader
        (apply combine-chunks
               (concat
                (list (prewalk
                       (fn [subexp]
                         (if (seq? subexp)
                           (let [f (first subexp)
                                 macro-fn (or (when macros
                                                (macros f))
                                              (when-not exclude-defaults?
                                                (default-macros f)))]
                             (if macro-fn
                               (let [macro-result (apply macro-fn
                                                         (rest subexp))]
                                 (if (map? macro-result)
                                   (do (swap! chunks
                                              conj
                                              (:chunk macro-result))
                                       (:expression macro-result))
                                   macro-result))
                               subexp))
                           subexp))
                       shader))
                @chunks))]
    (if (= new-shader shader)
      new-shader
      (apply-macros new-shader exclude-defaults?))))

(defn wort->wgsl [{:keys [fragment vertex compute]
                   :as shader}]
  (-> shader
      (cond-> fragment
        (update :functions
                assoc
                'fragment
                (cons 'fragment fragment)))
      (cond-> vertex
        (update :functions
                assoc
                'vertex
                (cons 'vertex vertex)))
      (cond-> compute
        (update :functions
                assoc
                'compute
                (cons 'compute compute)))
      apply-macros
      core-wort->wgsl))
