;; Copyright © 2020, JUXT LTD.

(ns crux.pull.eql-server-test
  (:require
   [crux.pull.eql-graphql-test :refer [introspection-query]]
   [clojure.test :refer [deftest is testing]]
   [crux.pull.http-client :as http]
   [clojure.pprint :refer [pprint]]
   [jsonista.core :as json])
  (:import
   (java.net.http HttpRequest$BodyPublishers HttpResponse)))

(comment
  (let [client (http/new-client)]
    (http/request
     client
     :post "http://localhost:8081/graphql"
     {:async false
      :request-body-publisher
      (HttpRequest$BodyPublishers/ofString
       (json/write-value-as-string
        {"operationName" "IntrospectionQuery"
         "query" introspection-query}))
      :on-success
      (fn [^java.net.http.HttpResponse result]
        (assert (= 200 (.statusCode result)))
        (let [json (json/read-value (.body result))]
          (pprint json)
          ))})))
