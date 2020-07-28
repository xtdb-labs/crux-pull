;; Copyright © 2020, JUXT LTD.

(ns crux.pull.eql-query-test
  (:require
   [clojure.test :refer [deftest is use-fixtures run-tests successful?]]
   [edn-query-language.core :as eql]

   [clojure.string :as str]))

(do
  (defn graphql-name [k]
    (if (namespace k)
      (format
       "%s__%s"
       (str/replace (namespace k) "." "_")
       (str/replace (name k) "." "_"))
      (str/replace (name k) "." "_")))

  (defn to-field [child]
    (case (:type child)
      :prop {"name" (graphql-name (:dispatch-key child))
             "description" nil
             ;; Any parameters? Check EDN list
             "args" []}))
  (defn type-finder [node]
    (case (:type node)
      :root
      (concat
       [{"kind" "OBJECT"
         "name" "Root"
         "description" nil
         ;; TODO: find fields
         "fields" (mapv to-field (:children node))
         "inputFields" nil
         "interfaces" nil
         "enumValues" nil
         "possibleTypes" nil}]
       (mapcat type-finder (:children node)))
      :prop [{"kind" "SCALAR"
              "name" (graphql-name (:dispatch-key node))
              "description" nil
              "fields" nil
              "inputFields" nil
              "interfaces" nil
              "enumValues" nil
              "possibleTypes" nil}]))

  (deftest to-field-test
    (is (= 3 (count
              (type-finder
               (eql/query->ast [:album/name :album/year]))))))

  (assert (successful? (run-tests)))

  (eql/query->ast [:album/name :album/year])

  )

#_'[{(:all-people
      {:resolver
       {:crux/query
        {:find [?p]
         :where [[?p :person/name ?name]]}
        :debug false}
       :graphql/name "allPeople"
       :graphql/description "Get all the people"
       :graphql/type "Person"
       })
     [(:person/name {:description "A person's name"
                     :graphql/name "person_name"})
      (:person/email {:description "A person's email"
                      :graphql/name "person_email"})]}]
