;; Copyright © 2020, JUXT LTD.

(ns crux.pull.graphql-test
  (:require
   [juxt.reap.alpha.graphql :as reap-graphql]
   [juxt.reap.alpha.api :as reap]
   [crux.pull.alpha.eql.exec :as exec]
;;   [crux.pull.alpha.eql.graphql :as graphql]
   [clojure.test :refer [deftest is successful? run-tests]]
   [clojure.string :as str]
   [edn-query-language.core :as eql]
))

(do
  ;; An idea for a directive
  (reap/decode reap-graphql/Document "query @at(validTime:\"2019-10-10\") { greeting }")

  ;; Parse this {greeting} into EQL

  ;; This AST defines the schema. It frames what we can query for.

  (defn eql-keyword [s]
    (when s
      (let [[ns n z] (str/split s #"__")]
        (when z (throw (ex-info "Can only have one double-underscore in a name" {})))
        (if n
          (keyword
           (str/replace ns "_" ".")
           (str/replace n "_" "."))
          (keyword (str/replace ns "_" "."))))))

  (deftest eql-keyword-test
    (is (= :a/b (eql-keyword "a__b")))
    (is (= :a.b/c.d (eql-keyword "a_b__c_d")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (eql-keyword "zip_foo__foo_bar__zip"))))

  (defmulti selection-to-eql-term first)

  (defmethod selection-to-eql-term :field [[_ m]] (eql-keyword (:name m)))

  (defn graphql-query-to-eql-ast [query-doc]
    (let [op
          (first
           (reap/decode reap-graphql/Document query-doc))
          _ (assert (= (:operation-type op) "query"))
          selection-set (:selection-set op)
          eql (mapv selection-to-eql-term selection-set)]

      (eql/query->ast eql)))

  (defn eql-query
    "Query with the given EQL AST, against the given schema, with the given
  resolver."
    [ast schema resolver options]

    ;; The given ast needs to be executed 'within the bounds of', or 'framed by'
    ;; the 'schema' ast (with validation)

    ;; Ultimately, this calls exec/exec
    (into {} (exec/exec resolver nil ast options)))

  (deftest graphql-query-to-eql-ast-test
    (is
     (=
      {:type :root
       :children
       [{:type :prop :dispatch-key :greeting :key :greeting}
        {:type :prop :dispatch-key :audience :key :audience}]}
      (graphql-query-to-eql-ast
       "query { greeting audience }"))))

  (deftest graphql-query-execution-test
    (is
     (=
      {:greeting "Hello" :audience "World"}
      (eql-query
       (graphql-query-to-eql-ast
        "query { greeting audience }")

       (eql/query->ast
        '[:greeting :audience])

       (reify exec/Resolver
         (lookup [_ ctx property opts]
           (get {:greeting "Hello"
                 :audience "World"} property)))

       {}))))

  (assert (successful? (run-tests)))

  ;; Let's try to query for the __schema

  (reap/decode
   reap-graphql/Document
   "query IntrospectionQuery {
      __schema {
        queryType { name }
        types {
          kind
          name
        }
      }
    }
  ")
  )
