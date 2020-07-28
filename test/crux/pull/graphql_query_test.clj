;; Copyright © 2020, JUXT LTD.

(ns crux.pull.graphql-query-test
  (:require
   [juxt.reap.alpha.graphql :as reap-graphql]
   [juxt.reap.alpha.api :as reap]
   [crux.pull.alpha.eql.exec :as exec]
   [edn-query-language.core :as eql]
   [clojure.test :refer [deftest is run-tests successful? testing]]
   [clojure.string :as str]))

;; An idea for a directive
(reap/decode reap-graphql/Document "query @at(validTime:\"2019-10-10\") { greeting }")

;; Parse this {greeting} into EQL

;; This AST defines the schema. It frames what we can query for.

(do
  (defn eql-keyword [s]
    (when s
      (let [[ns n z] (str/split s #"__")]
        (when z (throw (ex-info "Can only have one double-underscore in a name" {})))
        (if n
          (keyword
           (str/replace ns "_" ".")
           (str/replace n "_" "."))
          (keyword (str/replace ns "_" "."))
          )
        )
      ))

  (deftest eql-keyword-test
    (is (= :a/b (eql-keyword "a__b")))
    (is (= :a.b/c.d (eql-keyword "a_b__c_d")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (eql-keyword "zip_foo__foo_bar__zip"))))

  (defmulti selection-to-eql-term first)

  (defmethod selection-to-eql-term :field [[_ m]] (eql-keyword (:name m)))

  (deftest graphql-query-execution-test
    (is
     (=
      {:greeting "Hello"
       :audience "World"}
      (let [op
            (first
             (reap/decode reap-graphql/Document "query { greeting audience }"))
            selection-set (:selection-set op)]
        (assert (= (:operation-type op) "query"))
        (let [eql (mapv selection-to-eql-term selection-set)
              ast (eql/query->ast eql)]
          (into {}
                (exec/exec
                 (reify exec/Resolver
                   (lookup [_ ctx property opts]
                     (get {:greeting "Hello"
                           :audience "World"} property)))
                 nil
                 ast
                 {})))))))

  (assert (successful? (run-tests)))

  )
