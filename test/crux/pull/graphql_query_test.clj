;; Copyright © 2020, JUXT LTD.

(ns crux.pull.graphql-query-test
  (:require
   [juxt.reap.alpha.graphql :as reap-graphql]
   [juxt.reap.alpha.api :as reap]
   [crux.pull.alpha.eql.exec :as exec]
   [crux.pull.alpha.eql.graphql :as graphql]
   [clojure.test :refer [deftest is]]))

;; An idea for a directive
(reap/decode reap-graphql/Document "query @at(validTime:\"2019-10-10\") { greeting }")

;; Parse this {greeting} into EQL

;; This AST defines the schema. It frames what we can query for.

(deftest eql-keyword-test
  (is (= :a/b (graphql/eql-keyword "a__b")))
  (is (= :a.b/c.d (graphql/eql-keyword "a_b__c_d")))
  (is (thrown? clojure.lang.ExceptionInfo
               (graphql/eql-keyword "zip_foo__foo_bar__zip"))))

(deftest graphql-query-execution-test
  (is
   (=
    {:greeting "Hello"
     :audience "World"}
    (graphql/query
     "query { greeting audience }"
     (reify exec/Resolver
       (lookup [_ ctx property opts]
         (get {:greeting "Hello"
               :audience "World"} property)))
     {}))))
