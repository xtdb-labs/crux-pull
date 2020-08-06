;; Copyright © 2020, JUXT LTD.

(ns crux.pull.graphql-test
  (:require
   [jsonista.core :as json]
   [crux.pull.alpha.eql.graphql :as graphql]
   [crux.pull.alpha.eql :refer [prepare-query eql-query]]
   [clojure.test :refer [deftest is]]
   [edn-query-language.core :as eql]))

(deftest eql-keyword-test
  (is (= :a/b (graphql/eql-keyword "a__b")))
  (is (= :a.b/c.d (graphql/eql-keyword "a_b__c_d")))
  (is (= :__schema (graphql/eql-keyword "__schema")))
  (is (thrown? clojure.lang.ExceptionInfo
               (graphql/eql-keyword "zip_foo__foo_bar__zip"))))

(deftest parse-graphql-test
  (is
   (= [{:operation-type "query",
        :name "A",
        :selection-set [[:field {:name "foo", :arguments {}}]]}
       {:operation-type "query",
        :name "B",
        :selection-set [[:field {:name "bar", :arguments {}}]]}]
      (graphql/parse-graphql "query A {foo} query B {bar}"))))

(deftest operations-test
  (is
   (=
    [{:operation-type "query" :selection-set []}]
    (graphql/operations (graphql/parse-graphql " { }"))))
  (is
   (=
    [{:operation-type "query" :selection-set []}
     {:operation-type "query" :name "foo" :selection-set []}]
    (graphql/operations (graphql/parse-graphql " { } query foo {}")))))

;; TODO: validate-graphql-document-test

(deftest graphql-operation-test
  (is
   (=
    {:operation-type "query"}
    (graphql/graphql-operation
     [{:operation-type "query"}])))
  (is
   (=
    {:operation-type "query"}
    (graphql/graphql-operation
     [{:operation-type "query"}
      {:fragment-name "foo"}])))
  (is
   (=
    {:operation-type "query"}
    (graphql/graphql-operation
     [{:operation-type "query"
       :name "foo"}
      {:operation-type "query"}
      {:fragment-name "bar"}])))
  (is
   (=
    {:operation-type "query"
     :name "foo"}
    (graphql/graphql-operation
     [{:operation-type "query"
       :name "foo"}
      {:operation-type "query"}
      {:fragment-name "bar"}]
     "foo"))))

(deftest graphql-query-to-eql-ast-test
  (is
   (=
    {:type :root
     :children
     [{:type :prop :dispatch-key :greeting :key :greeting}
      {:type :prop :dispatch-key :audience :key :audience}]}
    (eql/query->ast
     (graphql/graphql-query-to-eql
      "query { greeting audience }")))))

(deftest graphql-query-execution-test
  (is
   (=
    {:greeting "Hello" :audience "World"}
    (eql-query
     (prepare-query
      (eql/query->ast
       (graphql/graphql-query-to-eql
        "query { greeting audience }"))
      (eql/query->ast
       '[:greeting :audience]))

     (reify crux.pull.alpha.eql/Resolver
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
     (mapv graphql/selection-to-eql-term)
     eql/query->ast))))

(deftest graphql-join-with-projection
  (is
   (=
    {:favoriteAlbums
     [#:album{:name "In Rainbows" :year 2007}
      #:album{:name "OK Computer" :year 1997}
      #:album{:name "Kraftwerk" :year 1974}]}

    (eql-query
     (eql/query->ast
      (graphql/graphql-query-to-eql
       "query { favoriteAlbums { album__name album__year }}"))

     (reify crux.pull.alpha.eql/Resolver
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
        (eql/query->ast
         (graphql/graphql-query-to-eql
          "query { favoriteAlbums { album__name album__year }}"))

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

       (reify crux.pull.alpha.eql/Resolver
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
        (eql/query->ast
         (graphql/graphql-query-to-eql
          "query { favoriteAlbums { album__name album__year }}"))

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

       (reify crux.pull.alpha.eql/Resolver
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
        (eql/query->ast
         (graphql/graphql-query-to-eql
          "query { favoriteAlbums(artist: \"Radiohead\") { album__name album__year }}"))

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

       (reify crux.pull.alpha.eql/Resolver
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
                        {:crux.graphql/description "Filter albums that have an `album__artist` fields which matches this argument value, if given."
                         :crux.graphql/type
                         {:kind "SCALAR"
                          :name "String"}}}})])

          types (graphql/eql-ast-node-to-graphql-types schema)

          ;; Add in EQL schema to support GraphQL introspection
          schema (graphql/add-introspection schema)]

      (json/read-value
       (json/write-value-as-string
        {:data
         (eql-query
          (prepare-query
           (eql/query->ast
            (graphql/graphql-query-to-eql introspection-query "IntrospectionQuery"))
           schema)

          ;; This is a GraphQL-aware resolver, which can infer implicit types and
          ;; respond to queries on them
          (reify crux.pull.alpha.eql/Resolver
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
