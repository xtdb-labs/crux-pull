;; Copyright © 2020, JUXT LTD.

(ns crux.pull.alpha.eql.exec)

;; In this ns, the following abbreviations are used:
;; resolver = something that can resolve queries against a data source, or proxy across multiple resolvers
;; ctx = execution context, containing the current graph node anchoring the sub-query
;; ast = the EQL AST of the query/sub-query
;; opts = per-query options

(defprotocol Resolver
  (lookup [_ ctx ast opts] "Resolve the given ast against the given ctx"))

(defmulti exec (fn [resolver ctx ast opts] (:type ast)))

(defmethod exec :root [resolver ctx ast opts]
  (vec (keep #(exec resolver ctx % opts) (:children ast))))

(defmethod exec :prop [resolver ctx ast opts]
  (when-let [v (lookup resolver ctx ast opts)]
    [(:key ast) v]))

(defmethod exec :join [resolver ctx ast opts]
  (if (get-in ast [:meta :single])
    (when-let [single (lookup resolver ctx ast opts)]
      (when (sequential? single)
        (throw
         (ex-info
          "Resolver returned a sequential when EQL AST marked as ^:single"
          {:single single
           :ctx ctx
           :ast ast})))
      [(:key ast)
       ;; This destroys order. Consider using an ordered map
       ;; (e.g. org.flatland/ordered "1.5.2")
       (into {} (keep #(exec resolver single % opts) (:children ast)))])
    (when-let [coll (lookup resolver ctx ast opts)]
      (when (not (sequential? coll))
        (throw
         (ex-info
          "Resolver must return a sequential unless EQL AST marked as ^:single"
          {:coll coll
           :ctx ctx
           :ast ast})))
      [(:key ast)
       (for [i coll]
         ;; This destroys order. Consider using an ordered map
         ;; (e.g. org.flatland/ordered "1.5.2")
         (into {} (keep #(exec resolver i % opts) (:children ast))))])))

;; TODO: Field Ordering
;; TODO: Result Coercion
