;; Copyright © 2020, JUXT LTD.

(ns crux.pull.eql-query-test
  (:require
   [clojure.test :refer [deftest is run-tests successful? testing]]
   [crux.pull.alpha.eql.exec :as exec]
   [edn-query-language.core :as eql]))

(deftest root-property-test
  (is
   (=
    [[:greeting "Hello World!"]]
    (exec/exec
     (reify exec/Resolver
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
      (exec/exec
       (reify exec/Resolver
         (lookup [_ ctx property opts]
           (get {:greeting "Hello World!"} property ::not-found)))
       nil
       (eql/query->ast '[:greeting :dummy])
       {})))))

(comment
  (do
    (assert (successful? (run-tests)))

    (exec/exec
     (reify exec/Resolver
       (lookup [_ ctx property opts]
         (get {:greeting "Hello World!"} property)))
     nil
     (eql/query->ast '[:greeting :dummy])
     {})
    ))
