;; Copyright © 2020, JUXT LTD.

(ns crux.pull.server
  (:require
   [integrant.core :as ig]
   [ring.adapter.jetty :as jetty]
   [crux.pull.alpha.eql :refer [prepare-query eql-query]]
   [crux.pull.alpha.graphql :as graphql]
   [jsonista.core :as json]
   [edn-query-language.core :as eql]
   [crux.api :as crux]
   [clojure.java.io :as io]
   [clojure.edn :as edn])
  (:import
   (crux.pull.alpha.eql Resolver)))

(comment
  (map first
       (crux/q (crux/db (dev/crux))
               '{:find [(eql/project ?f [:crux.db/id
                                         :film/name
                                         :film/year
                                         :film/box
                                         {:film/vehicles
                                          [:vehicle/brand :vehicle/model]}])]
                 :where [[?f :type :film]
                         ]
                 })))


(comment
  {:crux.db/id :film/name
   :crux.relation/type :crux.relation/number}

  ;; graphql types
  {:crux.db/id :film/vehicle
   :crux.relation/type :crux.relation/reference}

  {:crux.tx/put [:malcolm :foo/bar 123]}

  ;; rdf property, crux attribute
  {:crux.db/id :film/name
   :label "Name"
   :crux.relation/unique? true}

  (def my-relation
    '{:crux.db/id :malcolms/films

      :crux.relation/attributes ;; graphql fields
      {:film/name {:crux.graphql/name "filmName"
                   :crux.relation/required? true
                   :ui/label "Foo"
                   :crux.relation/cardinality :one}
       :film/vehicle {:crux.relation/required? true
                      :crux.relation/cardinality :many
                      ;; the values that attribute reference can be constrained
                      ;; here, optionally. ;; this is 'typeref' in graphql
                      :crux.relation/range :james-or-malcolms/vehicles}
       :film/bond-girl {:crux.relation/cardinality :zero-or-more}}

      :crux.relations/entities
      ;; extensional
      ;; #{}
      ;; intensional
      {:find [?e]
       :args [?subject]
       ;; Additional where clauses
       :where [
               [?e :james/type :film]
               ;; implicitly, all the required? true of the above
               ;; :crux.type/attributes will be added here
               [?e :film/name]
               [?e :film/vehicle]
               ]
       :rules []}})


  ;; Tentative : subclassing
  (def my-good-films
    '{:crux.db/id :good-films

      :crux.type/subclass :james/films

      :crux.type/attributes
      {:film/name {:type String}
       :film/vehicles {:type :vehicle}}

      :crux.type/entities
      {:find [?e]
       :args [?subject]
       :where [
               [?subject :likes ?e]
               (or
                [?e :malcolm/films]
                [?e :james/films])
               [(isa? :jamesbond/films ?e)]
               ]
       :rules []}}))


(defmethod ig/init-key ::server [_ {:keys [crux] :as config}]
  (crux/submit-tx
   crux
   (for [record (edn/read-string (slurp (io/resource "james-bond.edn")))]
     [:crux.tx/put record]))

  (let [schema
        (eql/query->ast
         ^{:crux.graphql/description
           "This Crux GraphQL demo uses the James Bond films dataset used by the Crux Console."}
         [#_^{
            :lookup
            (fn [ctx ast opts]
              (let [[_ eql] (first (eql/ast->expr ast))]
                (println "eql is" eql)
                (map
                 first
                 (crux/q
                  (:crux/db opts)
                  {:find [(list 'eql/project '?f eql)]
                   :where [
                           ['?f :type :film]

                           ['?f :film/name]
                           ['?f :film/director]


                           ]}))))
            :crux.graphql/description "These are the 007 films"}
          #_{:allFilms
           ^{:type :film} [:film/name
                :film/year
                :film/box
                {:film/vehicles
                 [:vehicle/brand :vehicle/model
                  {:film/_vehicles []}]
                 }]
           }

          ^{:lookup
            (fn [ctx ast opts]
              (let [[_ eql] (first (eql/ast->expr ast))]
                (println "eql is" eql)
                (map
                 first
                 (crux/q
                  (:crux/db opts)
                  {:find [(list 'eql/project '?f eql)]
                   :where [['?f :type :vehicle]]}))))
            :crux.graphql/description "These are the 007 vehicles"}
          {:vehicles
           [:vehicle/brand]
           }])

        types (graphql/eql-ast-node-to-graphql-types schema)

        schema (graphql/add-introspection schema)]

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
               (json/read-value (slurp (:body req)))]

           {:status 200
            :headers {"content-type" "application/json"}
            :body (json/write-value-as-string
                   {:data
                    (-> query
                        (graphql/graphql-query-to-eql operation)
                        eql/query->ast
                        (prepare-query schema)
                        (eql-query
                         ;; Build a resolver
                         (graphql/graphql-resolver types)
                         ;; Pass options
                         {:crux/db db}))})})
         {:status 404
          :body "Not Found"}))
     (conj config [:join? false]))))

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
