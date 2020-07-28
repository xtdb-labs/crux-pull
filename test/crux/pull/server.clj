;; Copyright © 2020, JUXT LTD.

(ns crux.pull.server
  (:require
   [integrant.core :as ig]
   [ring.adapter.jetty :as jetty]
   [crux.api :as crux]))

(defmethod ig/init-key ::server [_ {:keys [crux]}]
  (jetty/run-jetty
   (fn [req]
     {:status 200
      :body (pr-str (crux/status crux))})
   {:port 8080
    :join? false}))

(defmethod ig/halt-key! ::server [_ server]
  (.stop server))
