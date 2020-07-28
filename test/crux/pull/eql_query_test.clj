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
    (lookup [_ ctx property]
      (case [ctx property]
        [nil :greeting] "Hello World!")))

  (defmulti exec (fn [resolver ctx ast opts] (:type ast)))

  (defmethod exec :root [resolver ctx ast opts]
    (mapv #(exec resolver ctx % opts) (:children ast)))

  (defmethod exec :prop [resolver ctx ast opts]
    [(:key ast) (lookup resolver ctx (:key ast))])

  (deftest root-property-test
    (is
     (=
      [[:greeting "Hello World!"]]
      (exec
       (reify Resolver
         (lookup [_ ctx property]
           (case [ctx property]
             [nil :greeting] "Hello World!")))
       nil
       (eql/query->ast '[:greeting])
       {}))))

  (assert (successful? (run-tests)))

  (exec
   (reify Resolver
     (lookup [_ ctx property]
       (case [ctx property]
         [nil :greeting] "Hello World!")))

   nil
   (eql/query->ast '[:greeting])
   {})

  )
