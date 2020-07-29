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
  (when-let [coll (lookup resolver ctx ast opts)]
    [(:key ast)
     (for [i coll]
       (into {}
             (for [child (:children ast)]
               (exec resolver i child opts))))]))

;; TODO: Field Ordering
;; TODO: Result Coercion
