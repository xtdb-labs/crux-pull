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
   [clojure.edn :as edn]))

(defmethod ig/init-key ::server [_ {:keys [crux] :as config}]
  (crux/submit-tx
   crux
   (for [record (edn/read-string (slurp (io/resource "james-bond.edn")))]
     [:crux.tx/put record]))

  (let [schema
        (eql/query->ast
         ^{:crux.graphql/description
           "This Crux GraphQL demo uses the James Bond films dataset used by the Crux Console."}
         [^{:lookup
            (fn [ctx ast opts]
              (map #(zipmap [:name :director] %)
                   (crux/q
                    (:crux/db opts)
                    '{:find [?name ?director]
                      :where [[?f :type :film]
                              [?f :film/director ?director]
                              [?f :film/name ?name]]})))
            :graphql/type {:description "These are the 007 films"}}
          {:films
           [:name
            ^{:lookup
              (fn [ctx ast opts]
                (map #(zipmap [:name] %)
                     (crux/q
                      (:crux/db opts)
                      {:find '[?name]
                       :where [['?director :crux.db/id (:director ctx)] ['?director :person/name '?name]]})))}
            {:director [:name]}]}]
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
             [:name :director]}])

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
