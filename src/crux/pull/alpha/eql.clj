;; Copyright © 2020, JUXT LTD.

(ns crux.pull.alpha.eql)

(defmulti validate (fn [schema ast opts] (:type ast)))

(defmethod validate :root [schema ast opts]
  (seq (vec (keep #(validate (set (:children schema)) % opts) (:children ast)))))

(defmethod validate :prop [schema ast opts]
  (when-not (contains? schema ast)
    {:error {:message "Property not in schema"
             :property (:key ast)}}))

(defmethod validate :join [schema ast opts]
  :TODO
  )

(defmulti prepare-query (fn [ast schema] (:type ast)))

  (defn prepare-query-join [ast schema]
    (cond-> ast
      (:meta schema) (conj ast [:meta (:meta schema)])
      (:children ast)
      (update
       :children
       (fn [children]
         (vec
          (for [child children
                :let [child-schema
                      ;; Lookup appropriate schema
                      (some
                       (fn [sch]
                         (when (= (:dispatch-key child) (:dispatch-key sch))
                           sch))
                       (:children schema))]]
            (if child-schema
              (prepare-query
               child child-schema)
              ;; If there is no schema, the query is trying to go beyond the
              ;; frame of the schema
              (throw
               (ex-info
                "Validation error, query violates schema"
                {:query child
                 :schema-keys (map :dispatch-key (:children schema))})))))))))

  (defmethod prepare-query :root [ast schema]
    (prepare-query-join ast schema))

  (defmethod prepare-query :join [ast schema]
    (prepare-query-join ast schema))

  (defmethod prepare-query :prop [ast schema]
    (cond-> ast
      (:meta schema) (conj ast [:meta (:meta schema)])))

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

;; The schema determines what is possible to query. The schema also provides
;; information, via metadata, of how to resolve the query at each node. This
;; information is provided to the query, to equip it with the means to execute
;; itself unaided. This is also the step where extra properties can be deduced,
;; for example, for introspection.


(defn eql-query
  "Query with the given EQL AST, against the given schema, with the given
  resolver."
  [ast resolver options]
  (into {} (exec resolver nil ast options)))
