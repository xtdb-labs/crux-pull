;; Copyright © 2020, JUXT LTD.

(ns crux.pull.alpha.eql.exec)

;; In this ns, the following abbreviations are used:
;; resolver = something that can resolve queries against a data source, or proxy across multiple resolvers
;; ctx = execution context, containing the current graph node anchoring the sub-query
;; ast = the EQL AST of the query/sub-query
;; opts = per-query options

(defprotocol Resolver
  (lookup [_ ctx property opts] "Resolve the property against the given ctx"))

(defmulti exec (fn [resolver ctx ast opts] (:type ast)))

(defmethod exec :root [resolver ctx ast opts]
    (vec (keep #(exec resolver ctx % opts) (:children ast))))

  (defmethod exec :prop [resolver ctx ast opts]
    (when-let [v (lookup resolver ctx (:key ast) opts)]
      [(:key ast) v]))
