;; Copyright © 2020, JUXT LTD.

(ns crux.pull.eql-query-test
  (:require
   [clojure.test :refer [deftest is run-tests successful? testing]]
   [edn-query-language.core :as eql]
   [clojure.string :as str]))

(do
  (defn graphql-name [k]
    (when k
      (if (namespace k)
        (format
         "%s__%s"
         (str/replace (namespace k) "." "_")
         (str/replace (name k) "." "_"))
        (str/replace (name k) "." "_"))))

  (defn eql-ast-node-to-graphql-field
    "Each field has a kind, name, description"
    [child]
    (case (:type child)
      (:join :prop)
      {"name" (graphql-name (:dispatch-key child))
       "description" nil
       ;; Any parameters? Check EDN list
       "args" []}
      (throw
       (ex-info
        "Cannot convert to field, type not matched"
        {:type (:type child)}))))

  (defn eql-ast-node-to-graphql-type
    "A type in GraphQL has a mandatory kind:

  SCALAR, OBJECT, INTERFACE, UNION, ENUM, INPUT_OBJECT, LIST, NON_NULL.

  See https://spec.graphql.org/June2018/#sec-Schema-Introspection for full
  details."
    [node]
    (let [description (get-in node [:meta :graphql/type :description])]

      (cond->
          {"kind"
           (case (:type node)
             :root "OBJECT"
             ;; In EQL, this is the furthest we can go, but it doesn't imply that
             ;; the value returned is a scalar, it could be a map!
             :prop "SCALAR"
             ;; A join is always an object
             :join "OBJECT"
             (throw
              (ex-info
               "Cannot infer GraphQL type kind, EQL type not matched"
               {:type (:type node)})))}

          true
          (conj ["name"
                 (case (:type node)
                   :root "Root"
                   (or
                    (get-in node [:meta :graphql/type :name])
                    (graphql-name (:dispatch-key node))))])

          description
          (conj ["description" description])))

    #_(case (:type node)
        :root
        {"kind" "OBJECT"
         "name" "Root"
         "description" nil
         "fields" (mapv eql-ast-node-to-graphql-field (:children node))
         "inputFields" nil
         "interfaces" nil
         "enumValues" nil
         "possibleTypes" nil}
        :prop
        {"kind" "SCALAR"
         "name" (graphql-name (:dispatch-key node))
         "description" nil
         "fields" nil
         "inputFields" nil
         "interfaces" nil
         "enumValues" nil
         "possibleTypes" nil}
        :join
        {"kind" "OBJECT"
         "name" (graphql-name (:dispatch-key node))
         "description" nil
         "fields" nil
         "inputFields" nil
         "interfaces" nil
         "enumValues" nil
         "possibleTypes" nil}
        (throw
         (ex-info
          "Cannot extract GraphQL type, EQL type not matched"
          {:type (:type node)}))))

  (defn eql-ast-node-to-graphql-types [node]
    (case (:type node)
      :root
      (concat
       [(eql-ast-node-to-graphql-type node)]
       (mapcat eql-ast-node-to-graphql-types (:children node)))
      :prop
      [(eql-ast-node-to-graphql-type node)]
      :join
      [(eql-ast-node-to-graphql-type node)]
      (throw
       (ex-info
        "Cannot extract GraphQL type, EQL type not matched"
        {:type (:type node)}))))

  (deftest to-field-test
    (let [types (vec
                 (eql-ast-node-to-graphql-types
                  (eql/query->ast
                   [:album/name :album/year])))]
      (is (=
           (+ 1 ;; root
              2 ;; properties
              )
           (count types)))
      (is (= "Root" (get-in types [0 "name"]) ))
      (is (= "album__name" (get-in types [1 "name"]) ))
      (is (= "album__year" (get-in types [2 "name"]) )))

    (testing "with metadata it is possible to override the GraphQL type's name"
      (let [types
            (vec
             (eql-ast-node-to-graphql-types
              (eql/query->ast
               [:album/name
                (with-meta
                  '(:album/year)
                  {:graphql/type {:name "released"}})])))]

        (is (= "released" (get-in types [2 "name"]) ))))

    (testing "with metadata it is possible to override the GraphQL type's description"
      (let [types (vec
                   (eql-ast-node-to-graphql-types
                    (eql/query->ast
                     [:album/name
                      (with-meta
                        '(:album/year)
                        {:graphql/type
                         {:name "released"
                          :description "The year the album was released"}})])))]

        (is (nil? (get-in types [1 "description"]) ))
        (is (= "The year the album was released" (get-in types [2 "description"]) )))))

  (assert (successful? (run-tests)))

  (eql-ast-node-to-graphql-types
   (eql/query->ast [{:favorite-albums
                     [:album/name :album/year]}]))

  (eql-ast-node-to-graphql-types
   (eql/query->ast [:album/name :album/year])))

#_'[{(:all-people
      {:resolver
       {:crux/query
        {:find [?p]
         :where [[?p :person/name ?name]]}
        :debug false}
       :graphql/name "allPeople"
       :graphql/description "Get all the people"
       :graphql/type "Person"
       })
     [(:person/name {:description "A person's name"
                     :graphql/name "person_name"})
      (:person/email {:description "A person's email"
                      :graphql/name "person_email"})]}]
