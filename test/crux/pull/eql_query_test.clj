;; Copyright © 2020, JUXT LTD.

(ns crux.pull.eql-query-test
  (:require
   [clojure.test :refer [deftest is run-tests successful? testing]]
   [edn-query-language.core :as eql]))

;; resolver = something that can resolve queries against a data source, or proxy across multiple resolvers
;; ctx = execution context, containing the current graph node anchoring the sub-query
;; ast = the EQL AST of the query/sub-query
;; opts = per-query options

(do
  (defprotocol Resolver
    (lookup [_ ctx property opts]
      (case [ctx property]
        [nil :greeting] "Hello World!")))

  (defmulti exec (fn [resolver ctx ast opts] (:type ast)))

  (defmethod exec :root [resolver ctx ast opts]
    (vec (keep #(exec resolver ctx % opts) (:children ast))))

  (defmethod exec :prop [resolver ctx ast opts]
    (when-let [v (lookup resolver ctx (:key ast) opts)]
      [(:key ast) v]))

  (deftest root-property-test
    (is
     (=
      [[:greeting "Hello World!"]]
      (exec
       (reify Resolver
         (lookup [_ ctx property opts]
           (case [ctx property]
             [nil :greeting] "Hello World!")))
       nil
       (eql/query->ast '[:greeting])
       {})))

    (testing "non-existent property"
      (is
       (=
        [[:greeting "Hello World!"]
         [:dummy ::not-found]]
        (exec
         (reify Resolver
           (lookup [_ ctx property opts]
             (get {:greeting "Hello World!"} property ::not-found)))
         nil
         (eql/query->ast '[:greeting :dummy])
         {})))))

  (assert (successful? (run-tests)))

  (exec
   (reify Resolver
     (lookup [_ ctx property opts]
       (get {:greeting "Hello World!"} property)))
   nil
   (eql/query->ast '[:greeting :dummy])
   {})

  )
