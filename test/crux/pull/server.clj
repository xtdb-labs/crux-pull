;; Copyright © 2020, JUXT LTD.

(ns crux.pull.server
  (:require
   [integrant.core :as ig]
   [ring.adapter.jetty :as jetty]
   [jsonista.core :as json]
   [crux.api :as crux]
   [clojure.java.io :as io]))

(defmethod ig/init-key ::server [_ {:keys [crux] :as config}]
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
         (case operation
           "IntrospectionQuery"
           {:status 200
            :headers {"content-type" "application/json"}
            :body (json/write-value-as-string
                   {"data"
                    {"__schema"
                     {"queryType"
                      {"name" "Root"}
                      "mutationType" nil
                      "subscriptionType" nil
                      "types"
                      [{"kind" "OBJECT"
                        "name" "Root"
                        "description" nil
                        "fields" []
                        "inputFields" nil
                        "interfaces" []
                        "enumValues" nil
                        "possibleTypes" nil}]
                      "directives"
                      []}}})}

           {:status 200
            :headers {"content-type" "application/json"}
            :body (json/write-value-as-string
                   {"data"
                    {}})}))

       {:status 404
        :body "Not Found"}))
   (conj config [:join? false])))

(defmethod ig/halt-key! ::server [_ server]
  (.stop server))
