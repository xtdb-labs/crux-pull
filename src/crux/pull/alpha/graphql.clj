;; Copyright © 2020, JUXT LTD.

(ns crux.pull.alpha.graphql
  (:require
   [juxt.reap.alpha.api :as reap]
   [juxt.reap.alpha.graphql :as reap-graphql]))

(defn parse-graphql
  "Return a document"
  [s]
  (reap/decode reap-graphql/Document s))

(defn operations
    "Return just the operations of a document, expanding any shorthand
    definition."
    [graphql-doc]
    (->> graphql-doc
         (map
          (fn [definition]
            (if (= (keys definition) [:selection-set])
              (into {:operation-type "query"} definition)
              definition)))
         (filter :operation-type)))

(defn validate-graphql-document
  "'If a Document contains only one operation, that operation may be unnamed or
  represented in the shorthand form, which omits both the query keyword and
  operation name. Otherwise, if a GraphQL Document contains multiple operations,
  each operation must be named.'"
  [doc]
  (when-not
      (or
       (= (count (filter #(= (:operation-type %) "query") (operations doc))) 1)
       (every? :name (operations doc)))
      (throw (ex-info "Invalid GraphQL document" {:doc doc})))
  doc)


(defn
  ^{:graphql/name "GetOperation"}
  get-operation
  ([doc]
   (when-let [op
              (let [ops (filter #(= (:operation-type %) "query") (operations doc))]
                (if (= (count ops) 1)
                  (first ops)
                  (get-operation doc nil)))]
     op))
  ([doc op-name]
   (when-let [op (some #(when (= (:name %) op-name) %) doc)]
     op)))
