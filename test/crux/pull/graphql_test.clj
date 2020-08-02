;; Copyright © 2020, JUXT LTD.

(ns crux.pull.graphql-test
  (:require
   [juxt.reap.alpha.graphql :as reap-graphql]
   [juxt.reap.alpha.graphql.util :as reap-graphql-util]
   [crux.pull.graphql-introspection :as introspection]
   [jsonista.core :as json]
   [juxt.reap.alpha.api :as reap]
   [crux.pull.alpha.eql.exec :as exec]
   [crux.pull.eql-types-test :refer [eql-ast-node-to-graphql-types]]
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
        (cond
          ;; We want 'reserved' GraphQL keywords such as __schema to result in
          ;; :__schema
          (and ns (str/blank? ns))
          (keyword (str "__" n))
          ;; Input contains a __ which we convert to a namespaced keyword
          n
          (keyword
           (str/replace ns "_" ".")
           (str/replace n "_" "."))
          ;; Input doesn't contain __, so convert to a non-namespaced keyword
          :else
          ;; TODO: Convert camel casing to kebab case
          (keyword (str/replace ns "_" "."))))))

  (deftest eql-keyword-test
    (is (= :a/b (eql-keyword "a__b")))
    (is (= :a.b/c.d (eql-keyword "a_b__c_d")))
    (is (= :__schema (eql-keyword "__schema")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (eql-keyword "zip_foo__foo_bar__zip"))))

  (defn selection-to-eql-term [[_ {:keys [name arguments directives selection-set]}]]
    (let [add-arguments (fn [x] (if (not-empty arguments) (list x arguments) x))]
      (if selection-set
        {(add-arguments (eql-keyword name)) (mapv selection-to-eql-term selection-set)}
        (add-arguments (eql-keyword name)))))

  (defn graphql-query-to-eql [query-doc]
    (let [doc (reap/decode reap-graphql/Document query-doc)
          doc (reap-graphql-util/deref-fragments doc)
          ;; TODO: Currently we're just ripping out the first document, but the
          ;; GraphQL introspection query _tells_ us which name of query to use!
          op (first doc)
          _ (assert (= (:operation-type op) "query"))

          selection-set (:selection-set op)
          eql (mapv selection-to-eql-term selection-set)]
      eql))

  (defn graphql-query-to-eql-ast [query-doc]
    (eql/query->ast (graphql-query-to-eql query-doc)))

  ;; The schema determines what is possible to query. The schema also provides
  ;; information, via metadata, of how to resolve the query at each node. This
  ;; information is provided to the query, to equip it with the means to execute
  ;; itself unaided. This is also the step where extra properties can be
  ;; deduced, for example, for introspection.
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

  (deftest prepare-query-test
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
          result (prepare-query query schema)]

      (is result)
      (is (get-in result [:children 0 :meta :lookup]))
      (is (get-in result [:children 0 :children 0 :meta :lookup]))
      (is (get-in result [:children 0 :children 1 :meta :lookup]))))

  (defn eql-query
    "Query with the given EQL AST, against the given schema, with the given
  resolver."
    [ast resolver options]
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
       (prepare-query
        (graphql-query-to-eql-ast
         "query { greeting audience }")
        (eql/query->ast
         '[:greeting :audience]))

       (reify exec/Resolver
         (lookup [_ ctx ast opts]
           (get {:greeting "Hello"
                 :audience "World"} (:key ast))))

       {}))))

  (deftest graphql-selection-to-eql-term-test
    (is
     (=
      {:type :root
       :children
       [{:type :join
         :dispatch-key :favorite-albums
         :key :favorite-albums
         :query [:album/name :album/year]
         :children
         [{:type :prop :dispatch-key :album/name :key :album/name}
          {:type :prop :dispatch-key :album/year :key :album/year}]}]}

      (->>
       [{:operation-type "query"
         :selection-set
         [[:field
           {:name "favorite-albums"
            :arguments {}
            :selection-set
            [[:field {:name "album__name" :arguments {}}]
             [:field {:name "album__year" :arguments {}}]]}]]}]
       first
       :selection-set
       (mapv selection-to-eql-term)
       eql/query->ast
       ))))

  (deftest graphql-join-with-projection
    (is
     (=
      {:favoriteAlbums
       [#:album{:name "In Rainbows" :year 2007}
        #:album{:name "OK Computer" :year 1997}
        #:album{:name "Kraftwerk" :year 1974}]}

      (eql-query
       (graphql-query-to-eql-ast
        "query { favoriteAlbums { album__name album__year }}")

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

  (deftest graphql-prepared-query-lookup-test
    (is
     (=
      {:favoriteAlbums
       [#:album{:name "In Rainbows" :year 2007}
        #:album{:name "OK Computer" :year 1997}
        #:album{:name "Kraftwerk" :year 1974}]}
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
         (prepare-query
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
                         results)})]))

         (reify exec/Resolver
           (lookup [_ ctx ast opts]
             ((:lookup (:meta ast)) ctx)))
         {})))))

  (deftest graphql-query-prepare-query-substructure-lookup-test
    (is
     (=
      {:favoriteAlbums
       [#:album{:name "In Rainbows" :year 2007}
        #:album{:name "OK Computer" :year 1997}
        #:album{:name "Kraftwerk" :year 1974}]}
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
         (prepare-query
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
                         results)})]))

         (reify exec/Resolver
           (lookup [_ ctx ast opts]
             ((:lookup (:meta ast)) ctx)))
         {})))))

  (deftest graphql-filter-by-arguments-test
    (is
     (=
      {:favoriteAlbums
       [#:album{:name "In Rainbows" :year 2007}
        #:album{:name "OK Computer" :year 1997}]}

      (let [results
            [{:album/name "In Rainbows"
              :album/artist "Radiohead"
              :album/year 2007}
             {:album/name "OK Computer"
              :album/artist "Radiohead"
              :album/year 1997}
             {:album/name "Autobahn"
              :album/artist "Kraftwerk"
              :album/year 1974}]]

        (eql-query
         (prepare-query
          (graphql-query-to-eql-ast
           "query { favoriteAlbums(artist: \"Radiohead\") { album__name album__year }}")

          (eql/query->ast
           [(with-meta
              {:favoriteAlbums
               [(with-meta '(:album/name)
                  {:lookup (fn [ctx _] (get ctx :album/name))})
                (with-meta '(:album/artist)
                  {:lookup (fn [ctx _] (get ctx :album/artist))})
                (with-meta '(:album/year)
                  {:lookup (fn [ctx _] (get ctx :album/year))})]}

              {:lookup (fn [_ ast]
                         (if-let [artist (get-in ast [:params "artist"])]
                           (filter
                            (fn [album] (= (:album/artist album) artist))
                            results)
                           results))})]))

         (reify exec/Resolver
           (lookup [_ ctx ast opts]
             ((:lookup (:meta ast)) ctx ast)))

         {})))))

  ;; GraphiQL introspection query on init
  (def introspection-query
    "query IntrospectionQuery {
      __schema {

        queryType { name }
        mutationType { name }
        subscriptionType { name }
        types {
          ...FullType
        }
        directives {
          name
          description

          locations
          args {
            ...InputValue
          }
        }
      }
    }

    fragment FullType on __Type {
      kind
      name
      description
      fields(includeDeprecated: true) {
        name
        description
        args {
          ...InputValue
        }
        type {
          ...TypeRef
        }
        isDeprecated
        deprecationReason
      }
      inputFields {
        ...InputValue
      }
      interfaces {
        ...TypeRef
      }
      enumValues(includeDeprecated: true) {
        name
        description
        isDeprecated
        deprecationReason
      }
      possibleTypes {
        ...TypeRef
      }
    }

    fragment InputValue on __InputValue {
      name
      description
      type { ...TypeRef }
      defaultValue
    }

    fragment TypeRef on __Type {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                  ofType {
                    kind
                    name
                  }
                }
              }
            }
          }
        }
      }
    }
  ")

  (deftest introspection-query-test
    (is
     (=
      {"data"
       {"__schema"
        {"queryType" {"name" "Root"},
         "types"
         [{"interfaces" [],
           "name" "Root",
           "kind" "OBJECT",
           "fields"
           [{"args"
             [{"name" "artist",
               "type" {"name" "String", "kind" "SCALAR"},
               "description"
               "Filter albums that have an `album__artist` fields which matches this argument value, if given."}],
             "name" "favoriteAlbums",
             "type" {"name" "favoriteAlbums", "kind" "OBJECT"}}]}
          {"interfaces" [],
           "name" "favoriteAlbums",
           "kind" "OBJECT",
           "fields"
           [{"args" [],
             "name" "album__name",
             "type" {"name" "String", "kind" "SCALAR"}}
            {"args" [],
             "name" "album__artist",
             "type" {"name" "String", "kind" "SCALAR"}}
            {"args" [],
             "name" "album__year",
             "type" {"name" "String", "kind" "SCALAR"}}]}
          {"name" "String", "kind" "SCALAR"}]}}}
      (let [schema
            (eql/query->ast
             [(with-meta
                {:favoriteAlbums
                 [(with-meta '(:album/name) {})
                  (with-meta '(:album/artist) {})
                  (with-meta '(:album/year) {})]}
                {:params {:artist
                          {:graphql/description "Filter albums that have an `album__artist` fields which matches this argument value, if given."
                           :graphql/type
                           {:kind "SCALAR"
                            :name "String"}}}})])

            types (eql-ast-node-to-graphql-types schema)

            ;; Add in EQL schema to support GraphQL introspection
            schema (introspection/add-introspection schema)]

        (json/read-value
         (json/write-value-as-string
          {:data
           (eql-query
            (prepare-query
             (graphql-query-to-eql-ast introspection-query)
             schema)

            ;; This is a GraphQL-aware resolver, which can infer implicit types and
            ;; respond to queries on them
            (reify exec/Resolver
              (lookup [_ ctx ast opts]
                (if (= (:key ast) :__schema)
                  {:queryType {:name "Root"}
                   :mutationType nil
                   :subscriptionType nil
                   :types types}

                  ;; Default resolution if no lookup schema in schema is as
                  ;; follows:
                  (case (:type ast)
                    :join
                    (get ctx (:key ast))
                    :prop (get ctx (:key ast))
                    ))))
            {})}))))))

  (assert (successful? (run-tests)))
  )
