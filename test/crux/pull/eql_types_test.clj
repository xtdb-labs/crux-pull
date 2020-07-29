;; Copyright © 2020, JUXT LTD.

(ns crux.pull.eql-types-test
  (:require
   [clojure.test :refer [deftest is run-tests successful? testing]]
   [edn-query-language.core :as eql]
   [clojure.string :as str]))

(comment
  (do
    (defn graphql-name [k]
      (when k
        (if (namespace k)
          (format
           "%s__%s"
           (str/replace (namespace k) "." "_")
           (str/replace (name k) "." "_"))
          (str/replace (name k) "." "_"))))

    (declare eql-ast-node-to-graphql-type)

    (defn eql-ast-node-to-graphql-field
      "Each field has a kind, name, description"
      [node]
      (let [description (get-in node [:meta :graphql/field :description])]
        (cond-> {"name"
                 (or
                  (get-in node [:meta :graphql/field :name])
                  (graphql-name (:dispatch-key node)))}

          description
          (conj ["description" description])

          ;; Required array of InputValue!
          ;; Any parameters? Check EDN list
          true (conj ["args" []])

          ;; Types are required. We define 'inline', based on the set of available
          ;; children. There is an important question about whether types can be
          ;; embedded in schema queries, or whether they must be extracted out to
          ;; the top level and referenced where used. The SWAPI example does the
          ;; latter, and we're not yet sure whether GraphiQL requires this or
          ;; not. The only way to be sure is to work on the basis that inline
          ;; types aren't possible (although they appear to be in the spec), get a
          ;; demo working and testable against GraphiQL, and then later refactor
          ;; to try out the change to inline types. Note that in EQL, there are NO
          ;; types to worry about.
          true
          (conj ["type" (eql-ast-node-to-graphql-type node)])

          true (conj ["isDeprecated" false])
          false (conj ["deprecationReason" nil]))))

    (defn eql-ast-node-to-graphql-type
      "A type in GraphQL is required to have a kind:

  SCALAR, OBJECT, INTERFACE, UNION, ENUM, INPUT_OBJECT, LIST, NON_NULL.

  See https://spec.graphql.org/June2018/#sec-Schema-Introspection for full
  details."
      [node]
      (let [kind (case (:type node)
                   :root "OBJECT"
                   ;; In EQL, this is the furthest we can go, but it doesn't imply that
                   ;; the value returned is a scalar, it could be a map!
                   :prop "SCALAR"
                   ;; A join is always an object
                   :join "OBJECT"
                   (throw
                    (ex-info
                     "Cannot infer GraphQL type kind, EQL type not matched"
                     {:type (:type node)})))
            description (get-in node [:meta :graphql/type :description])]

        (cond->
            {"kind" kind}

            true
            (conj ["name"
                   (case (:type node)
                     :root "Root"
                     :prop "String"
                     :join
                     (or
                      (get-in node [:meta :graphql/type :name])
                      (graphql-name (:dispatch-key node))))])

            description
            (conj ["description" description])

            (#{"OBJECT" "INTERFACE"} kind)
            (conj ["fields" (mapv eql-ast-node-to-graphql-field (:children node))]))))

    (defn eql-ast-node-to-graphql-types [node]
      (distinct
       (case (:type node)
         :root
         (concat
          [(eql-ast-node-to-graphql-type node)]
          (mapcat eql-ast-node-to-graphql-types (:children node)))
         :prop
         [(eql-ast-node-to-graphql-type node)]
         :join
         (concat
          [(eql-ast-node-to-graphql-type node)]
          (mapcat eql-ast-node-to-graphql-types (:children node)))
         (throw
          (ex-info
           "Cannot extract GraphQL type, EQL type not matched"
           {:type (:type node)})))))

    (deftest eql-ast-node-to-graphql-types-test
      (testing "Root"
        (let [types (vec
                     (eql-ast-node-to-graphql-types
                      (eql/query->ast [])))]
          (is (= 1 (count types)))
          (is (= {"kind" "OBJECT"
                  "name" "Root"
                  "fields" []}
                 (first types))))

        (testing "setting the Root object type's description"
          (let [types (vec
                       (eql-ast-node-to-graphql-types
                        (eql/query->ast
                         (with-meta
                           []
                           {:graphql/type {:description "This is the root"}}))))]
            (is (= 1 (count types)))
            (is (= {"kind" "OBJECT"
                    "name" "Root"
                    "fields" []
                    "description" "This is the root"}
                   (first types))))))

      (testing "Properties"
        (let [types (vec
                     (eql-ast-node-to-graphql-types
                      (eql/query->ast
                       [:album/name :album/year])))]
          (is (=
               (+ 1 ;; root
                  1 ;; scalar string
                  )
               (count types)))

          ;; Expecting a Root OBJECT type, followed by two SCALAR types.
          (is (= ["OBJECT" "SCALAR"] (map #(get % "kind") types)))
          (is (= "Root" (get-in types [0 "name"])))
          (is (= "String" (get-in types [1 "name"]))))

        (testing "override the GraphQL field's name with metadata"
          (let [types
                (vec
                 (eql-ast-node-to-graphql-types
                  (eql/query->ast
                   [:album/name
                    (with-meta
                      '(:album/year)
                      {:graphql/field {:name "released"}})])))]

            (is (= "released" (get-in types [0 "fields" 1 "name"])))))

        (testing "provide the GraphQL field's description with metadata"
          (let [types (vec
                       (eql-ast-node-to-graphql-types
                        (eql/query->ast
                         [:album/name
                          (with-meta
                            '(:album/year)
                            {:graphql/field
                             {:name "released"
                              :description "The year the album was released"}})])))]

            (is (= "The year the album was released"
                   (get-in types [0 "fields" 1 "description"]) )))))

      (testing "Joins"
        (let [types
              (vec
               (eql-ast-node-to-graphql-types
                (eql/query->ast
                 [{:favorite-albums
                   [:album/name :album/year]}])))]

          ;; Expecting a root OBJECT type, one for favorite-albums, and the SCALAR String
          (is
           (=
            ["OBJECT" "OBJECT" "SCALAR"]
            (map #(get % "kind") types))))))

    (assert (successful? (run-tests)))

    [(eql/query->ast [{:favorite-albums
                       [:album/name :album/year]}])
     :=>
     (eql-ast-node-to-graphql-types
      (eql/query->ast (with-meta
                        [{:favorite-albums
                          [:album/name :album/year]}]
                        {:graphql/type {:description "The root object"}})))]))
