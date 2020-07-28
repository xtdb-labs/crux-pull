;; Copyright © 2020, JUXT LTD.

(ns crux.pull.server
  (:require
   [integrant.core :as ig]
   [ring.adapter.jetty :as jetty]
   [crux.api :as crux]
   [clojure.java.io :as io]))

(defmethod ig/init-key ::server [_ {:keys [crux] :as config}]
  (jetty/run-jetty
   (fn [req]
     (case (:uri req)
       "/" {:status 200
            :headers {"content-type" "text/html;charset=utf-8"}
            :body (slurp (io/resource "index.html"))}
       {:status 404
        :body "Not Found"}))
   (conj config [:join? false])))

(defmethod ig/halt-key! ::server [_ server]
  (.stop server))
