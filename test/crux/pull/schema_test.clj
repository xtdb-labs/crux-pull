;; Copyright © 2020, JUXT LTD.

(ns crux.pull.schema-test
  (:require
   [crux.api :as crux]
   ;;[jsonista.core :as json]
   [crux.pull.alpha.eql.graphql :as graphql]
   [crux.pull.graphql-test :refer [introspection-query]]
   [cheshire.core :as json]
   [clojure.test :refer [deftest is are testing]]))


(comment
  (do

    (defn ^{:graphql/name "ExecuteQuery"}
      execute-query [query schema opts]
      :ok
      )

    (let [attribute-to-graphql-field
          (fn [attr]
            (let [{:crux.graphql/keys [name]
                   :crux.schema/keys [description]
                   :as args} attr]
              (prn attr description)
              ;; Name is mandatory, ex-info is missing!
              (cond-> {}
                true (conj
                      (if name
                        ["name" name]
                        (throw (ex-info "Missing :crux.graphql/name" {:attribute attr}))))
                description (conj ["description" description])
                ;; TODO: args
                ;; TODO: type
                true (conj {"isDeprecated" false})
                )))
          entity-to-graphql-type
          (fn [e]
            (cond
              (= e String)
              {"kind" "SCALAR" "name" "String"}
              (= e Long)
              {"kind" "SCALAR" "name" "Long"}
              (map? e)
              (let [{:crux.graphql/keys [name]
                     :crux.schema/keys [description attributes]
                     t :crux.schema/type}
                    e]
                (cond-> {}
                  t (conj ["kind" "OBJECT"])
                  name (conj ["name" name])
                  description (conj ["description" description])
                  attributes (conj ["fields" (mapv attribute-to-graphql-field (vals attributes))])
                  ))
              :else (throw (ex-info "Condition not supported" {:entity e})))
            )]
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
                   {:crux.graphql/name "filmName"
                    :crux.schema/required? true}
                   :film/box
                   {:crux.schema/type Long
                    :crux.schema/description "How much the film made at the box office"
                    :crux.schema/required? true
                    :crux.graphql/name "box"}
                   :film/cost
                   {:crux.schema/description "How much the film cost to make"
                    :crux.schema/type Long
                    :crux.schema/required? true
                    :crux.graphql/name "cost"}
                   :film/earnings
                   {:crux.schema/type String
                    :crux.schema/derived
                    ^:crux.memoize '(fn [{:film/keys [box cost]}] (- box cost))
                    :crux.graphql/name "earnings"}
                   :film/vehicle
                   {:crux.schema/type :ex.type/vehicle
                    :crux.schema/cardinality :crux.schema.cardinality/zero-or-more
                    :crux.graphql/name "vehicles"}}}

                 {:crux.db/id :ex.type/vehicle
                  :crux.schema/type :crux.schema.type/relation
                  :crux.graphql/name "Vehicle"
                  :crux.schema/attributes
                  {:vehicle/brand
                   {:crux.schema/type String
                    :crux.graphql/name "brand"}
                   :vehicle/model
                   {:crux.schema/type String
                    :crux.graphql/name "model"}}}

                 {:crux.db/id :ex.type/graphql-query-root
                  :crux.schema/type :crux.schema.type/relation
                  :crux.graphql/name "Root"
                  :crux.schema/attributes
                  {:all-films
                   {:crux.schema/description "All the films in the James Bond universe."
                    :crux.schema/type :ex.type/film
                    :crux.schema/cardinality :crux.schema.cardinality/zero-or-more
                    :crux.graphql/name "allFilms"
                    }
                   :film
                   {:crux.schema/description "A particular film in the James Bond universe."
                    :crux.schema/type :ex.type/film
                    :crux.graphql/name "film"}
                   }}

                 ]]
            [:crux.tx/put ent])))

        (let [db (crux/db node)
              schema {:types
                      (mapv entity-to-graphql-type
                            [(crux/entity db :ex.type/graphql-query-root)
                             ;; TODO: All the rest should be discovered via graph traversal
                             (crux/entity db :ex.type/film)
                             (crux/entity db :ex.type/vehicle)
                             String
                             ])}]

          ;; Extract GraphQL types to file, just to compare
          (spit "/tmp/schema.json"
                (json/generate-string
                 {:data
                  {"__schema" schema}}
                 {:pretty true}))

          ;; Query the schema with GraphQL execution

          (-> introspection-query
              graphql/parse-graphql
              graphql/validate-graphql-document
              (graphql/graphql-operation "IntrospectionQuery")
              ;; Attempt to execute this query against the schema
              (execute-query schema {:variables [] :initial-value :Root}))

          )))))

;; https://en.wikipedia.org/wiki/Functional_dependency
;; https://en.wikipedia.org/wiki/Armstrong%27s_axioms
;; https://en.wikipedia.org/wiki/The_Third_Manifesto
