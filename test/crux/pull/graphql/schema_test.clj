;; Copyright © 2020, JUXT LTD.

(ns crux.pull.graphql.schema-test
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [clojure.test :refer [deftest is are testing run-tests successful?]]
   [crux.api :as crux]
   [crux.pull.alpha.graphql :as grab]
   [crux.pull.alpha.graphql.crux-schema :as cgl]
   [crux.pull.eql-graphql-test :refer [introspection-query]]))

(deftest execute-test
  (is
   (map?
    (with-open
      [node (crux/start-node {:crux.node/topology '[crux.standalone/topology]})]
      (crux/await-tx
       node
       (crux/submit-tx
        node
        (for [ent
              [{
                ;; The Crux id also acts as the name of the relation schema
                ;; represented by this entity.
                :crux.db/id :ex.type/film
                :crux.schema/type :crux.schema.type/relation
                :crux.schema/description "A James Bond Film"
                :crux.graphql/name "Film"
                :crux.schema/attributes
                {:film/name
                 {:crux.schema/description "The film's title"
                  :crux.schema/type String
                  :crux.schema/cardinality :crux.schema.cardinality/one
                  :crux.schema/required? true
                  :crux.graphql/name "filmName"}
                 :film/year
                 {:crux.schema/description "The film's year"
                  :crux.schema/type String
                  :crux.schema/cardinality :crux.schema.cardinality/one
                  :crux.schema/required? false
                  :crux.graphql/name "year"}
                 :film/box
                 {:crux.schema/type Integer
                  :crux.schema/description "How much the film made at the box office"
                  :crux.schema/cardinality :crux.schema.cardinality/one
                  :crux.schema/required? false
                  :crux.graphql/name "box"}
                 :film/cost
                 {:crux.schema/type Integer
                  :crux.schema/description "How much the film cost to make"
                  :crux.schema/cardinality :crux.schema.cardinality/one
                  :crux.schema/required? false
                  :crux.graphql/name "cost"}
                 :film/earnings
                 {:crux.schema/type String
                  :crux.schema/cardinality :crux.schema.cardinality/one
                  :crux.schema/dependencies #{:film/box :film/cost}
                  :crux.schema/derived
                  ^:crux.memoize '(fn [{:film/keys [box cost]}] (- box cost))
                  :crux.graphql/name "earnings"}
                 :film/vehicles
                 {:crux.schema/type :ex.type/vehicle
                  :crux.schema/join :film/vehicles
                  :crux.schema/cardinality :crux.schema.cardinality/many
                  :crux.graphql/name "vehicles"}
                 :film/director
                 {:crux.schema/type :ex.type/director
                  :crux.schema/cardinality :crux.schema.cardinality/one
                  :crux.schema/required? false
                  :crux.graphql/name "director"}}}

               {:crux.db/id :ex.type/director
                :crux.schema/type :crux.schema.type/relation
                :crux.graphql/name "Director"
                :crux.schema/attributes
                {:person/name
                 {:crux.schema/type String
                  :crux.schema/cardinality :crux.schema.cardinality/one
                  :crux.schema/required? true
                  :crux.graphql/name "name"}}}

               {:crux.db/id :ex.type/vehicle
                :crux.schema/type :crux.schema.type/relation
                :crux.graphql/name "Vehicle"
                :crux.schema/attributes
                {:vehicle/brand
                 {:crux.schema/type String
                  :crux.schema/cardinality :crux.schema.cardinality/one
                  :crux.schema/required? true
                  :crux.graphql/name "brand"}
                 :vehicle/model
                 {:crux.schema/type String
                  :crux.schema/cardinality :crux.schema.cardinality/one
                  :crux.schema/required? true
                  :crux.graphql/name "model"}}}

               {:crux.db/id :ex.type/graphql-query-root
                :crux.schema/type :crux.schema.type/relation
                :crux.graphql/name "Root"
                :crux.schema/attributes
                {:all-films
                 {:crux.schema/description "All the films in the James Bond universe."
                  :crux.schema/type :ex.type/film
                  :crux.schema/cardinality :crux.schema.cardinality/many
                  :crux.graphql/name "allFilms"}
                 :film
                 {:crux.schema/description "A particular film in the James Bond universe."
                  ;; The type is the relation was are targetting.
                  :crux.schema/type :ex.type/film
                  :crux.schema/cardinality :crux.schema.cardinality/one

                  ;; GraphQL fields are the analog of Crux attributes. Fields have
                  ;; arguments, so makes sense to declare them here, at the
                  ;; attribute value level.
                  :crux.schema/arguments
                  {:id
                   {:crux.schema/join :crux.db/id
                    :crux.graphql/name "id"
                    :crux.schema/type String}

                   :name
                   {:crux.schema/join :film/name
                    :crux.schema/comparator :crux.schema.comparator/equals
                    :crux.graphql/name "name"
                    :crux.schema/type String}

                   :year
                   {:crux.schema/join :film/year
                    :crux.schema/comparator :crux.schema.comparator/equals
                    :crux.graphql/name "madeIn"
                    :crux.schema/type String}

                   :cost-more-than
                   {:crux.schema/join :film/cost
                    :crux.schema/description "A comparison query on cost"
                    :crux.schema/comparator :crux.schema.comparator/greater-than
                    :crux.graphql/name "costsMore"
                    :crux.schema/type Integer}}

                  :crux.graphql/name "film"}}}]]
          [:crux.tx/put ent])))

      (crux/await-tx
       node
       (crux/submit-tx
        node
        (for [record
              (edn/read-string
               (slurp
                (io/resource "james-bond.edn")))]
          [:crux.tx/put record])))



      (let [db (crux/db node)
            document
            (->
             #_"query { allFilms { filmName box year vehicles { brand model }}}"
             #_"query { film(name:\"For Your Eyes Only\") { filmName year }}"
             #_"query { film(madeIn:\"1963\") { filmName }}"
             introspection-query

             grab/parse-graphql
             grab/validate-graphql-document)

            ]

        ;; TODO: Introspection (Attempt to execute this query against the schema)

        ;; Schema is complected, separate into schema and 'type-resolver'

        (grab/execute-request
         {:schema (cgl/map->DbSchema
                   {"queryType" "Root"
                    :db db})
          :document document
          ;; TODO: Where was the original code written to determine the query to
          ;; run? Also, see spec advice on this.
          :operation-name "IntrospectionQuery"
          :variable-values {}
          :initial-value (crux/entity db :ex.type/graphql-query-root)

          :field-resolver
          (fn [{:keys [object-type field-name object-value argument-values] :as args}]

            (cond
              (= field-name "__schema")
              {"types" (mapv #(cgl/to-graphql-type db %) (cgl/visit-types db :ex.type/graphql-query-root #{}))
               ;; possible types
               "queryType" {"kind" "OBJECT"
                            "name" "Root"}
               "mutationType" nil
               "subscriptionType" nil
               "directives" []}

              (= field-name "__type")
              (throw (ex-info "TODO: __type" {}))

              (:crux.schema/entity object-type)
              (let [[attr-k attr]
                    (some
                     #(when (= (get (second %) :crux.graphql/name) field-name) %)
                     (get-in object-type [:crux.schema/entity :crux.schema/attributes]))

                    field-type (:crux.schema/type attr)]

                ;; For each arg in :crux.schema/arguments, go through with argument-values

                (cond

                  ;; A :crux.schema/type of a keyword, absolutely implies this is
                  ;; a reference to another relation. The cardinality is implied
                  ;; to be :crux.schema.cardinality/zero-or-more, but we should
                  ;; think about whether cardinality needs to be explicit in this
                  ;; case, and what other cardinalities would mean.
                  (keyword? field-type)
                  ;; Check type is list first?
                  (let [relation (crux/entity db field-type)

                        relation-required-attributes
                        (for [[k v] (:crux.schema/attributes relation)
                              :when (:crux.schema/required? v)]
                          k)

                        datalog
                        {:find ['?e]
                         :where (vec
                                 (cond->
                                     (for [k relation-required-attributes]
                                       ['?e k])

                                   (:crux.schema/join attr)
                                   (conj [(:crux.db/id object-value) (:crux.schema/join attr) '?e])

                                   (:crux.schema/arguments attr)
                                   (concat
                                    (keep (fn [[arg-k arg-v]]
                                            (when-let [[_ v] (find argument-values (:crux.graphql/name arg-v))]
                                              ['?e (:crux.schema/join arg-v) v]))
                                          (:crux.schema/arguments attr)))))}]

                    (for [ref (map first (crux/q db datalog))]
                      (crux/entity db ref)))

                  (#{String Integer} field-type)
                  ;; What if the :crux.schema/type of the field is different?
                  (get object-value attr-k)

                  :else
                  (throw
                   (ex-info
                    "TODO: resolve entity field"
                    {:attr-k attr-k
                     :field-name field-name
                     :object-value object-value
                     :object-type object-type
                     :attr attr
                     :field-type field-type}))))


              :else
              (if-let [[_ child] (find object-value field-name)]
                child

                nil
                #_(pprint
                   (ex-info
                    "TODO: resolve field"
                    {:field-name field-name
                     :object-value object-value
                     :object-type object-type
                     })))
              ))}))))))

;; https://en.wikipedia.org/wiki/Functional_dependency
;; https://en.wikipedia.org/wiki/Armstrong%27s_axioms
;; https://en.wikipedia.org/wiki/The_Third_Manifesto
