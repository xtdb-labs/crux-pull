;; Copyright © 2020, JUXT LTD.

(ns crux.pull.http-client
  (:import
   [java.net URI]
   [java.net.http HttpClient HttpClient$Version HttpRequest HttpResponse$BodyHandlers HttpRequest$BodyPublishers]))

(defn new-client
  ([]
   (new-client {}))
  ([{:keys [connect-timeout]
      :or {connect-timeout (java.time.Duration/ofSeconds 20)}
     :as opts}]
   (.. (HttpClient/newBuilder)
       (version HttpClient$Version/HTTP_1_1)
       (connectTimeout connect-timeout)
       (build))))

(defn new-request
  ([method url]
   (new-request method url {}))
  ([method url {:keys [request-body-publisher headers] :as opts}]
   (.build
    (cond-> (HttpRequest/newBuilder)
      (= method :get) (.GET)
      (= method :delete) (.DELETE)
      headers ((fn [builder]
                 (doseq [[k v] headers]
                   (.setHeader builder k v))
                 builder))
      request-body-publisher (.method (.toUpperCase (name method)) request-body-publisher)
      true (.uri (URI/create url))))))

(defn request
  ([client method url]
   (request client method url {}))
  ([client
    method
    url
    {:keys [response-body-handler async on-success on-error]
     :or {response-body-handler (HttpResponse$BodyHandlers/ofString)}
     :as opts}]
   (let [request (new-request method url opts)]
     (if async
       (cond->
           (.sendAsync client request response-body-handler)
           on-error (. (exceptionally
                        (reify java.util.function.Function
                          (apply [_ error]
                            (println "Error:" error)
                            (on-error error)))))
           on-success (. (thenAccept
                          (reify java.util.function.Consumer
                            (accept [_ result]
                              (on-success result))))))
       (try
         (let [result (.send client request response-body-handler)]
           (if on-success (on-success result) result))
         (catch Exception e
           (if on-error (on-error e) (throw e))))))))

(defn ->www-form-urlencoded [m]
  (apply
   str
   (interpose
    "&"
    (for [[k v] m]
      (if (sequential? v)
        (apply str (interpose "&" (for [v v] (str k "=" v))))
        (str k "=" v))))))
