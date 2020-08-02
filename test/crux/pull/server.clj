;; Copyright © 2020, JUXT LTD.

(ns crux.pull.server
  (:require
   [integrant.core :as ig]
   [ring.adapter.jetty :as jetty]
   [crux.pull.alpha.eql.exec :as exec]
   [crux.pull.graphql-test :as graphql]
   [crux.pull.graphql-introspection :as graphql-introspection]
   [jsonista.core :as json]
   [edn-query-language.core :as eql]
   [crux.api :as crux]
   [crux.pull.graphql-test :refer [prepare-query]]
   [crux.pull.eql-types-test :refer [eql-ast-node-to-graphql-types]]
   [clojure.java.io :as io]))

(defmethod ig/init-key ::server [_ {:keys [crux] :as config}]
  (let [schema
        (->
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
         graphql-introspection/add-introspection)

        types (eql-ast-node-to-graphql-types schema)]

    (jetty/run-jetty
     (fn [req]
       (case [(:request-method req) (:uri req)]
         [:get "/"]
         {:status 200
          :headers {"content-type" "text/html;charset=utf-8"}
          :body (slurp (io/resource "index.html"))}

         [:post "/graphql"]
         (let [{operation "operationName"
                query "query"}
               (json/read-value (slurp (:body req)))]

           {:status 200
            :headers {"content-type" "application/json"}
            :body (json/write-value-as-string
                   {:data
                    (graphql/eql-query

                     (prepare-query
                      (graphql/graphql-query-to-eql-ast query)
                      schema)

                     ;; Only works for schema queries
                     (reify exec/Resolver
                       (lookup [_ ctx ast opts]
                         (if (= (:key ast) :__schema)
                           {:queryType {:name "Root"}
                            :mutationType nil
                            :subscriptionType nil
                            :types types}

                           (case (:type ast)
                             :join
                             (get ctx (:key ast))
                             :prop (get ctx (:key ast))
                             ))))

                     #_(reify exec/Resolver
                         (lookup [_ ctx property opts]
                           (get {:greeting "Hello"
                                 :audience "World"} property)))
                     {})})})

         {:status 404
          :body "Not Found"}))
     (conj config [:join? false]))))

(defmethod ig/halt-key! ::server [_ server]
  (.stop server))
