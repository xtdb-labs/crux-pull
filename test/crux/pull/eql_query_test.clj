;; Copyright © 2020, JUXT LTD.

(ns crux.pull.eql-query-test
  (:require
   [clojure.test :refer [deftest is run-tests successful? testing]]
   [crux.pull.alpha.eql.exec :as exec]
   [crux.pull.alpha.eql.validate :as validate]
   [edn-query-language.core :as eql]))

(comment
  (do
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


    ;; Low-level: Call EQL directly, no validation, properties not found yield nil results.
    ;; Used by 'road builders' - schema construction
    (deftest eql-exec-test
      (is
       (=
        [[:greeting "Hello"] [:audience "World"]]
        (exec/exec
         (reify exec/Resolver
           (lookup [_ ctx property opts]
             (get {:greeting "Hello"
                   :audience "World"} property)))
         nil
         (eql/query->ast '[:greeting :dummy :audience])
         {}))))

    ;; Mid-level: Query with EQL, but the EQL is subject to validation according to the schema
    ;; Used by Clojure app developers, e.g. UI devs
    (deftest eql-validation-test
      (is
       (=
        [{:error {:message "Property not in schema", :property :dummy}}]
        (validate/validate
         (eql/query->ast '[:greeting :audience])
         (eql/query->ast '[:greeting :dummy :audience])
         {})))

      (is
       (nil?
        (validate/validate
         (eql/query->ast '[:greeting :audience])
         (eql/query->ast '[:greeting :audience])
         {}))))

    (assert (successful? (run-tests)))

    ;; Work on validation of joins


    ;; GraphQL-level: Query with GraphQL, calls down to 'mid-level'.

    ))
