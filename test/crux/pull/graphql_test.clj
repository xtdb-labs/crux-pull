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

  (defn selection-to-eql-term [[_ {:keys [name arguments directives selection-set]}]]
    (if selection-set
      {(eql-keyword name) (mapv selection-to-eql-term selection-set)}
      (eql-keyword name)))

  (defn graphql-query-to-eql-ast [query-doc]
    (let [op
          (first
           (reap/decode reap-graphql/Document query-doc))
          _ (assert (= (:operation-type op) "query"))
          selection-set (:selection-set op)
          eql (mapv selection-to-eql-term selection-set)]

      (eql/query->ast eql)))

  ;; The schema determines what is possible to query. The schema also provides
  ;; information, via metadata, of how to resolve the query at each node. This
  ;; information is provided to the query, to equip it with the means to help in
  ;; its execution.
  (defmulti transplant (fn [ast schema] (:type ast)))

  (defmethod transplant :root [ast schema]
    (cond-> ast
      (:meta schema) (conj ast [:meta (:meta schema)])
      (:children ast) (update :children (fn [children]
                                          (mapv transplant children (:children schema))))))

  (defmethod transplant :join [ast schema]
    (cond-> ast
      (:meta schema) (conj ast [:meta (:meta schema)])
      (:children schema) (update :children (fn [children]
                                             (mapv transplant children (:children schema))))))

  (defmethod transplant :prop [ast schema]
    (cond-> ast
      (:meta schema) (conj ast [:meta (:meta schema)])))

  (deftest transplant-test
    (let [results
          [{:album/name "In Rainbows"
            :album/artist "Radiohead"
            :album/year 2007}
           {:album/name "OK Computer"
            :album/artist "Radiohead"
            :album/year 1997}
           {:album/name "Kraftwerk"
            :album/artist "Autobahn"
            :album/year 1974}]
          schema
          (eql/query->ast
           [(with-meta
              {:favoriteAlbums
               [(with-meta '(:album/name)
                  {:lookup (fn [ctx] (get ctx :album/name))})
                (with-meta '(:album/artist)
                  {:lookup (fn [ctx] (get ctx :album/artist))})
                (with-meta '(:album/year)
                  {:lookup (fn [ctx] (get ctx :album/year))})
                ]}
              {:lookup (fn []
                         ;; For example, do some datalog query.
                         ;; For this test, just return results
                         results)})])
          query (graphql-query-to-eql-ast
                 "query { favoriteAlbums { album__name album__year }}")
          result (transplant query schema)]

      (is result)
      (is (get-in result [:children 0 :meta :lookup]))
      (is (get-in result [:children 0 :children 0 :meta :lookup]))
      (is (get-in result [:children 0 :children 1 :meta :lookup]))))

  (defn eql-query
    "Query with the given EQL AST, against the given schema, with the given
  resolver."
    [ast schema resolver options]

    ;; TODO: Add __schema, __type etc. to every relevant node in the schema to
    ;; provide automatic introspection.

    ;; The given ast needs to be executed 'within the bounds of', or 'framed by'
    ;; the 'schema' ast (with validation)

    ;; TODO: Validate the ast wrt. the schema

    ;; Ultimately, this calls exec/exec
    (into {} (exec/exec resolver nil (cond-> ast schema (transplant schema)) options)))

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
         (lookup [_ ctx ast opts]
           (get {:greeting "Hello"
                 :audience "World"} (:key ast))))

       {}))))

  (deftest graphql-selection-to-eql-term-test
    (is
     (=
      {:type :root,
       :children
       [{:type :join,
         :dispatch-key :favorite-albums,
         :key :favorite-albums,
         :query [:album/name :album/year],
         :children
         [{:type :prop, :dispatch-key :album/name, :key :album/name}
          {:type :prop, :dispatch-key :album/year, :key :album/year}]}]}

      (->>
       [{:operation-type "query",
         :selection-set
         [[:field
           {:name "favorite-albums",
            :arguments {},
            :selection-set
            [[:field {:name "album__name", :arguments {}}]
             [:field {:name "album__year", :arguments {}}]]}]]}]
       first
       :selection-set
       (mapv selection-to-eql-term)
       eql/query->ast
       ))))

  (deftest graphql-join-with-projection
    (is
     (=
      {:favoriteAlbums
       [#:album{:name "In Rainbows", :year 2007}
        #:album{:name "OK Computer", :year 1997}
        #:album{:name "Kraftwerk", :year 1974}]}

      (eql-query
       (graphql-query-to-eql-ast
        "query { favoriteAlbums { album__name album__year }}")

       ;; No schema yet
       nil

       (reify exec/Resolver
         (lookup [_ ctx ast opts]
           (case (:key ast)
             :album/name (get ctx :album/name)
             :album/artist (get ctx :album/artist)
             :album/year (get ctx :album/year)
             :favoriteAlbums
             [{:album/name "In Rainbows"
               :album/artist "Radiohead"
               :album/year 2007}
              {:album/name "OK Computer"
               :album/artist "Radiohead"
               :album/year 1997}
              {:album/name "Kraftwerk"
               :album/artist "Autobahn"
               :album/year 1974}])))
       {}))))

  (deftest graphql-query-with-transplanted-eql-schema-lookup-test
    (is
     (=
      {:favoriteAlbums
       [#:album{:name "In Rainbows", :year "Radiohead"}
        #:album{:name "OK Computer", :year "Radiohead"}
        #:album{:name "Kraftwerk", :year "Autobahn"}]}
      (let [results
            [{:album/name "In Rainbows"
              :album/artist "Radiohead"
              :album/year 2007}
             {:album/name "OK Computer"
              :album/artist "Radiohead"
              :album/year 1997}
             {:album/name "Kraftwerk"
              :album/artist "Autobahn"
              :album/year 1974}]]

        (eql-query
         (graphql-query-to-eql-ast
          "query { favoriteAlbums { album__name album__year }}")

         (eql/query->ast
          [(with-meta
             {:favoriteAlbums
              [(with-meta '(:album/name)
                 {:lookup (fn [ctx] (get ctx :album/name))})
               (with-meta '(:album/artist)
                 {:lookup (fn [ctx] (get ctx :album/artist))})
               (with-meta '(:album/year)
                 {:lookup (fn [ctx] (get ctx :album/year))})
               ]}
             {:lookup (fn [ctx]
                        ;; For example, do some datalog query.
                        ;; For this test, just return results
                        results)})])

         (reify exec/Resolver
           (lookup [_ ctx ast opts]
             ((:lookup (:meta ast)) ctx)))
         {})))))

  (assert (successful? (run-tests))))

;; TODO
;; "query { favoriteAlbums(artist: \"Radiohead\") { album__name album__year }}"

;; Let's try to query for the __schema
#_(reap/decode
     reap-graphql/Document
     "query {
      __schema {
        queryType { name }
        types {
          kind
          name
        }
      }
    }
  ")
