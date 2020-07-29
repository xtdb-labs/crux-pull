;; Copyright © 2020, JUXT LTD.

(ns crux.pull.alpha.eql.graphql
  (:require
   [juxt.reap.alpha.graphql :as reap-graphql]
   [juxt.reap.alpha.api :as reap]
   [edn-query-language.core :as eql]
   [crux.pull.alpha.eql.exec :as exec]
   [clojure.string :as str]))

(defn eql-keyword [s]
    (when s
      (let [[ns n z] (str/split s #"__")]
        (when z (throw (ex-info "Can only have one double-underscore in a name" {})))
        (if n
          (keyword
           (str/replace ns "_" ".")
           (str/replace n "_" "."))
          (keyword (str/replace ns "_" "."))))))

(defmulti selection-to-eql-term first)

(defmethod selection-to-eql-term :field [[_ m]] (eql-keyword (:name m)))

(defn query [query-doc resolver opts]
  (let [op
        (first
         (reap/decode reap-graphql/Document query-doc))
        _ (assert (= (:operation-type op) "query"))
        selection-set (:selection-set op)
        eql (mapv selection-to-eql-term selection-set)

        ast (eql/query->ast eql)]

    ;; This ast needs to be 'within the bounds of', or 'framed by' the 'schema' ast
    ;; validation

    (into {} (exec/exec resolver nil ast opts))))
