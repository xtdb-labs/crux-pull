;; Copyright © 2020, JUXT LTD.

(ns crux.pull.graphql-server
  (:require
   [integrant.core :as ig]
   [clojure.pprint :refer [pprint]]
   [ring.adapter.jetty :as jetty]
   [jsonista.core :as json]
   [crux.pull.alpha.graphql.crux-schema :as cgl]
   [crux.pull.alpha.graphql :as grab]
   [crux.api :as crux]
   [clojure.java.io :as io]
   [clojure.edn :as edn]))

(defmethod ig/init-key ::server [_ {:keys [crux] :as config}]
  (crux/submit-tx
   crux
   (for [record (edn/read-string (slurp (io/resource "james-bond.edn")))]
     [:crux.tx/put record]))

  (crux/await-tx
   crux
   (crux/submit-tx
    crux
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
             :film/director
             {:crux.schema/type :ex.type/director
              :crux.schema/join :film/director
              :crux.schema/cardinality :crux.schema.cardinality/one
              :crux.schema/required? false
              :crux.graphql/name "director"}
             :film/vehicles
             {:crux.schema/type :ex.type/vehicle
              :crux.schema/join :film/vehicles
              :crux.schema/cardinality :crux.schema.cardinality/many
              :crux.graphql/name "vehicles"}
             :film/bond-girls
             {:crux.schema/type :ex.type/person
              :crux.schema/join :film/bond-girls
              :crux.schema/cardinality :crux.schema.cardinality/many
              :crux.graphql/name "bondGirls"}
             }}

           {:crux.db/id :ex.type/director
            :crux.schema/type :crux.schema.type/relation
            :crux.graphql/name "Director"
            :crux.schema/attributes
            {:person/name
             {:crux.schema/type String
              :crux.schema/cardinality :crux.schema.cardinality/one
              :crux.schema/required? true
              :crux.graphql/name "name"}
             :films
             {:crux.schema/type :ex.type/film
              :crux.schema/backjoin :film/director
              :crux.schema/cardinality :crux.schema.cardinality/many
              :crux.schema/required? false
              :crux.graphql/name "films"}
             }}

           {:crux.db/id :ex.type/person
            :crux.schema/type :crux.schema.type/relation
            :crux.schema/description "An individual involved in one or more of the James Bond films"
            :crux.graphql/name "Person"
            :crux.schema/attributes
            {:person/name
             {:crux.schema/type String
              :crux.schema/description "The person's full name"
              :crux.schema/cardinality :crux.schema.cardinality/one
              :crux.schema/required? true
              :crux.graphql/name "name"}}}

           {:crux.db/id :ex.type/vehicle
            :crux.schema/type :crux.schema.type/relation
            :crux.schema/description "A vehicle used in one of the James Bond films"
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
            :crux.schema/description "Various queries on the James Bond universe"
            :crux.graphql/name "Root"
            :crux.schema/attributes
            {:all-films
             {:crux.schema/description "All the films in the James Bond universe."
              :crux.schema/type :ex.type/film
              :crux.schema/cardinality :crux.schema.cardinality/many
              :crux.graphql/name "allFilms"}
             :all-directors
             {:crux.schema/description "All the directors of James Bond films."
              :crux.schema/type :ex.type/director
              ;; No joins, just a constraint that something has to have a film/director attribute to this target
              :crux.schema/where [['?t :film/director '?e]]
              :crux.schema/cardinality :crux.schema.cardinality/many
              :crux.graphql/name "allDirectors"}
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

  (jetty/run-jetty
   (fn [req]

     (case ((juxt :request-method :uri) req)
       [:get "/"]
       {:status 200
        :headers {"content-type" "text/html;charset=utf-8"}
        :body (slurp (io/resource "index.html"))}

       [:post "/graphql"]
       (let [db (crux/db crux)
             {operation "operationName"
              query "query"}
             (json/read-value (slurp (:body req)))

             document
             (->
              query
              grab/parse-graphql
              grab/validate-graphql-document)]

         {:status 200
          :headers {"content-type" "application/json"}
          :body
          (json/write-value-as-string
           (try
             {:data
              (grab/execute-request
               {:schema (cgl/map->DbSchema
                         {"queryType" "Root"
                          :db db})
                :document document
                ;; TODO: Where was the original code written to determine the query to
                ;; run? Also, see spec advice on this.
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

                          cardinality (:crux.schema/cardinality attr)
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

                                           (:crux.schema/backjoin attr)
                                           (conj ['?e (:crux.schema/backjoin attr) (:crux.db/id object-value)])

                                           (:crux.schema/arguments attr)
                                           (concat
                                            (keep (fn [[arg-k arg-v]]
                                                    (when-let [[_ v] (find argument-values (:crux.graphql/name arg-v))]
                                                      ['?e (:crux.schema/join arg-v) v]))
                                                  (:crux.schema/arguments attr)))

                                           (:crux.schema/where attr)
                                           (concat (:crux.schema/where attr))))}]

                          (println "DATALOG, cardinality is" cardinality)
                          (pprint datalog)

                          (cond->
                              (for [ref (map first (crux/q db datalog))]
                                (let [r
                                      (crux/entity db ref)]
                                  #_(println "RESULT")
                                  #_(println r)
                                  r
                                  ))
                              (= cardinality :crux.schema.cardinality/one)
                              first
                              (= cardinality :crux.schema.cardinality/many)
                              vec))

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
                      nil)))})}
             (catch clojure.lang.ExceptionInfo e
               {:errors [{:error (.getMessage e)
                          :data (ex-data e)}]}
               )))})
       {:status 404
        :body "Not Found"}))
   (conj config [:join? false])))

(defmethod ig/halt-key! ::server [_ server]
  (.stop server))


#_[^{:graphql/type
              {:description "Get all Bond films"}
              :lookup
              (fn [ctx ast]
                (if-let [artist (get-in ast [:params "artist"])]
                  (filter
                   (fn [album] (= (:album/artist album) artist))
                   results)
                  results))
              #_:params
              #_{:artist
                 {:graphql/description "Filter albums that have an `album__artist` fields which matches this argument value, if given."
                  :graphql/type
                  {:kind "SCALAR"
                   :name "String"}}}}
            {:films
             [:name :director]}]
