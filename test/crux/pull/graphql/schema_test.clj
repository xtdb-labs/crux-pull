;; Copyright © 2020, JUXT LTD.

(ns crux.pull.graphql.schema-test
  (:require
   [crux.api :as crux]
   [crux.pull.alpha.graphql :as graphql]
   [clojure.java.io :as io]
   [crux.pull.eql-graphql-test :refer [introspection-query]]
   [clojure.edn :as edn]
   [cheshire.core :as json]
   [clojure.test :refer [deftest is are testing run-tests successful?]]
   [flatland.ordered.map :refer [ordered-map]]))

(comment
  (do

    (defprotocol Schema
      (lookup-type [_ type-name] "Return the GraphQL type"))

    (defprotocol Type
      (lookup-field [_ field-name] "Return the GraphQL field"))

    (defn validate-schema [schema]

      (when-not (get schema "queryType")
        (throw
         (ex-info
          "Schema validation error: queryType is required"
          {:schema schema})))

      (when-not (get schema "directives")
        (throw
         (ex-info
          "Schema validation error: directives is required"
          {:schema schema})))

      schema)

    (defn
      ^{:crux.graphql.spec-ref/version "June2018"
        :crux.graphql.spec-ref/section "6.3.2"
        :crux.graphql.spec-ref/algorithm "CollectFields"}
      collect-fields
      [{:keys [object-type selection-set variable-values visited-fragments]
        ;; 1. If visitedFragments if not provided, initialize it to the empty
        ;; set.
        :or {visited-fragments {}}}]

      (reduce
       (fn [grouped-fields [selection-type m]]
         (case selection-type
           :field

           (let [response-key
                 ;; TODO: The response-key will be the alias, if it exists
                 (:name m)]
             (update grouped-fields response-key (fnil conj []) m))

           :fragment-spread
           (throw (ex-info "TODO: fragment-spread" {}))

           :inline-fragment
           (throw (ex-info "TODO: inline-fragment" {}))))

       (ordered-map)                    ; grouped-fields
       selection-set))

    (defn
      ^{:crux.graphql.spec-ref/version "June2018"
        :crux.graphql.spec-ref/section "6.4.1"
        :crux.graphql.spec-ref/algorithm "CoerceArgumentValues"}
      coerce-argument-values
      [{:keys [object-type field variable-values]}]

      (let [
            ;; 1. Let coercedValues be an empty unordered Map.
            coerced-values {}
            ;; 2. Let argumentValues be the argument values provided in field.
            argument-values (:arguments field)
            ;; 3. Let fieldName be the name of field.
            field-name (:name field)
            ;; 4. Let argumentDefinitions be the arguments defined by objectType
            ;; for the field named fieldName.
            argument-definitions (get (lookup-field object-type field-name) "args")
            ]

        #_(reduce
           (fn [])
           argument-definitions)

        #_(throw (ex-info "TODO" {:field field
                                  :object-type object-type}))
        {}))

    (defn
      ^{:crux.graphql.spec-ref/version "June2018"
        :crux.graphql.spec-ref/section "6.4.3"
        :crux.graphql.spec-ref/algorithm "ResolveAbstractType"}
      resolve-abstract-type
      [{:keys [field-type result]}]
      (throw (ex-info "TODO" (meta #'resolve-abstract-type))))

    (defn
      ^{:crux.graphql.spec-ref/version "June2018"
        :crux.graphql.spec-ref/section "6.4.3"
        :crux.graphql.spec-ref/algorithm "MergeSelectionSets"}
      merge-selection-sets
      [{:keys [fields]}]
      (reduce
       (fn [selection-set field]
         (let [field-selection-set (:selection-set field)]
           (cond-> selection-set
             field-selection-set (concat field-selection-set))))
       (list)
       fields))

    (defn
      ^{:crux.graphql.spec-ref/version "June2018"
        :crux.graphql.spec-ref/section "6.4.2"
        :crux.graphql.spec-ref/algorithm "ResolveFieldValue"}
      resolve-field-value
      [{:keys [object-type object-value field-name argument-values field-resolver]}]
      (assert field-resolver)

      (println "Resolving field-name:" field-name)

      (field-resolver
       {:object-type object-type
        :field-name field-name
        :object-value object-value
        :argument-values argument-values}))

    (declare execute-selection-set-normally)

    (defn ^{:crux.graphql.spec-ref/version "June2018"
            :crux.graphql.spec-ref/section "6.4.3"
            :crux.graphql.spec-ref/algorithm "CompleteValue"}
      complete-value
      [{:keys [field-type fields result variable-values field-resolver]}]

      (println "Completing value, field-type is " field-type "result is" result)

      (cond
        ;; 1. If the fieldType is a Non‐Null type:
        (= (get field-type "kind") "NON_NULL")
        ;; a. Let innerType be the inner type of fieldType.
        (let [inner-type (get field-type "ofType")
              ;; b. Let completedResult be the result of calling
              ;; CompleteValue(…).
              completed-result
              (complete-value
               {:field-type inner-type
                :fields fields
                :result result
                :variable-values variable-values
                :field-resolver field-resolver})]
          ;; c. If completedResult is null, throw a field error.
          (when (nil? completed-result)
            (throw (ex-info "Field error" {:field-type inner-type})))
          ;; d. Return completedResult.
          completed-result)

        ;; 2. If result is null (or another internal value similar to null such
        ;; as undefined or NaN), return null.
        (nil? result) nil

        ;; 3. If fieldType is a List type:
        (= (get field-type "kind") "LIST")
        (do
          (println "A list...")
          ;; a. If result is not a collection of values, throw a field error.
          (when-not (sequential? result)
            (throw (ex-info "Resolver must return a collection" {:field-type field-type}))
            )
          ;; b. Let innerType be the inner type of fieldType.
          (let [inner-type (get field-type "ofType")]
            ;; c. Return a list where each list item is the result of calling
            ;; CompleteValue(innerType, fields, resultItem, variableValues),
            ;; where resultItem is each item in result.

            (for [result-item result]
              (complete-value
               {:field-type inner-type
                :fields fields
                :result result-item
                :variable-values variable-values
                :field-resolver field-resolver}))))

        ;; 4. If fieldType is a Scalar or Enum type:
        (#{"SCALAR" "ENUM"} (get field-type "kind"))
        ;; a. Return the result of “coercing” result, ensuring it is a legal value of fieldType, otherwise null.
        result

        ;; 5. If fieldType is an Object, Interface, or Union type:
        (#{"OBJECT" "INTERFACE" "UNION"} (get field-type "kind"))
        (let [object-type
              (if (= (get field-type "kind") "OBJECT")
                field-type
                (resolve-abstract-type
                 {:field-type field-type
                  :result result}))
              sub-selection-set (merge-selection-sets {:fields fields})]
          (execute-selection-set-normally
           {:selection-set sub-selection-set
            :object-type object-type
            :object-value result
            :variable-values variable-values
            :field-resolver field-resolver}))))

    (defn
      ^{:crux.graphql.spec-ref/version "June2018"
        :crux.graphql.spec-ref/section "6.4"
        :crux.graphql.spec-ref/algorithm "ExecuteField"}
      execute-field
      [{:keys [object-type object-value field-type fields variable-values field-resolver]}]

      (println "Execute field" )
      (println "object-type" (pr-str object-type))
      (println "object-value" (pr-str object-value))
      (println "fields" (pr-str fields))

      ;; 1. Let field be the first entry in fields.
      (let [field (first fields)
            ;; 2. Let fieldName be the field name of field.
            field-name (:name field)
            ;; 3. Let argumentValues be the result of CoerceArgumentValues(…).
            argument-values
            (coerce-argument-values
             {:object-type object-type
              :field field
              :variable-values variable-values})

            ;; 4. Let resolvedValue be ResolveFieldValue(…).
            resolved-value
            (resolve-field-value
             {:object-type object-type
              :object-value object-value
              :field-name field-name
              :argument-values argument-values
              :field-resolver field-resolver})]

        (prn "resolved-value" resolved-value)
        (prn "field-type" field-type)

        ;; 5. Return the result of CompleteValue(…).
        (complete-value
         {:field-type field-type
          :fields fields
          :result resolved-value
          :variable-values variable-values
          :field-resolver field-resolver})))

    (defn
      ^{:crux.graphql.spec-ref/version "June2018"
        :crux.graphql.spec-ref/section "6.3"
        :crux.graphql.spec-ref/algorithm "ExecuteSelectionSet"}
      execute-selection-set-normally
      "Return a map with :data and :errors."
      [{:keys [selection-set object-type object-value variable-values field-resolver]}]
      (println "selection-set" selection-set)

      ;; 1. Let groupedFieldSet be the result of CollectFields
      (let [grouped-field-set
            (collect-fields
             {:object-type object-type
              :selection-set selection-set
              :variable-values variable-values})
            ;; 2. Initialize resultMap to an empty ordered map.
            result-map (ordered-map)]

        ;; 3. For each groupedFieldSet as responseKey and fields:
        (reduce
         (fn [result-map [response-key fields]]

           (println "result-map:" [response-key fields])
           (println "object-type:" object-type)
           (println "object-type-type:" (type object-type))

           ;; a. Let fieldName be the name of the first entry in fields. Note:
           ;; This value is unaffected if an alias is used.
           (let [field-name (:name (first fields))
                 _ (println "Field name is" field-name)
                 _ (println "object-type is" object-type )
                 _ (println "type of object-type is" (type object-type))
                 ;; b. Let fieldType be the return type defined for the field fieldName of objectType.
                 field-type (get (lookup-field object-type field-name) "type")]

             (prn "field-type:" field-type)

             ;; c. If fieldType is defined:
             (if field-type
               ;; i. Let responseValue be ExecuteField(objectType, objectValue,
               ;; fields, fieldType, variableValues).
               (let [response-value
                     (execute-field
                      {:object-type object-type
                       :object-value object-value
                       :field-type field-type
                       :fields fields
                       :variable-values variable-values
                       :field-resolver field-resolver})]
                 ;; ii. Set responseValue as the value for responseKey in resultMap.
                 (conj result-map [response-key response-value]))
               ;; Otherwise return the accumulator
               result-map)))
         result-map
         grouped-field-set)))

    (defn
      ^{:crux.graphql.spec-ref/version "June2018"
        :crux.graphql.spec-ref/section "6.2.1"
        :crux.graphql.spec-ref/algorithm "ExecuteQuery"}
      execute-query
      [{:keys [query schema variable-values initial-value field-resolver]}]

      ;; 1. Let queryType be the root Query type in schema.
      (let [query-type-name (get schema "queryType")
            query-type (lookup-type schema query-type-name)]

        ;; 2. Assert: queryType is an Object type.
        (when-not (= (get query-type "kind") "OBJECT")
          (throw (ex-info
                  "Query type must be an OBJECT"
                  (into
                   {:query-type query-type
                    :crux.graphql.spec-ref/step 2}
                   (meta #'execute-query)))))

        ;; 3. Let selectionSet be the top level Selection Set in query.
        (let [selection-set (:selection-set query)]
          ;; 4. Let data be the result of running ExecuteSelectionSet
          ;; normally (allowing parallelization).
          ;; 5. Let errors be any field errors produced while executing the selection set.
          ;; 6. Return an unordered map containing data and errors.
          (execute-selection-set-normally
           {:selection-set selection-set
            :object-type query-type
            :object-value initial-value
            :variable-values variable-values
            :field-resolver field-resolver}))))

    (defn
      ^{:crux.graphql.spec-ref/version "June2018"
        :crux.graphql.spec-ref/section "6.1"
        :crux.graphql.spec-ref/algorithm "ExecuteRequest"}
      execute-request [{:keys [schema document operation-name variable-values initial-value field-resolver]}]
      ;; 1. Let operation be the result of GetOperation(document, operationName).
      (let [operation (graphql/get-operation document operation-name)
            ;; 2. Let coercedVariableValues be the result of
            ;; CoerceVariableValues(schema, operation, variableValues). (TODO)
            coerced-variable-values variable-values]

        (case (:operation-type operation)
          ;; 3. If operation is a
          "query" ;; operation:
          ;;   a. Return ExecuteQuery(operation, schema, coercedVariableValues,
          ;;   initialValue).
          (execute-query
           {:query operation
            :schema schema
            :variable-values coerced-variable-values
            :initial-value initial-value
            :field-resolver field-resolver})

          ;; 4. Otherwise if operation is a mutation operation:
          ;;   a. Return ExecuteMutation(operation, schema, coercedVariableValues, initialValue).

          ;; TODO

          ;; 5. Otherwise if operation is a subscription operation:
          ;;   a. Return Subscribe(operation, schema, coercedVariableValues, initialValue).

          ;; TODO

          (throw (ex-info "No operation type on operation" {:operation operation}))
          )))

    (defn- attribute-to-graphql-field [db attr]
      (let [{:crux.graphql/keys [name]
             :crux.schema/keys [description cardinality required?]
             t :crux.schema/type
             :as args} attr]
        ;; Name is mandatory, ex-info is missing!
        (cond-> {}
          true (conj
                (if name
                  ["name" name]
                  (throw (ex-info "Missing :crux.graphql/name" {:attribute attr}))))
          description (conj ["description" description])
          ;; TODO: args
          t (conj {"type"
                   (cond
                     (= t String)
                     (if required?
                       {"kind" "NON_NULL"
                        "name" nil
                        "ofType" {"kind" "SCALAR" "name" "String"}}
                       {"kind" "SCALAR" "name" "String"})
                     (= t Integer)
                     (if required?
                       {"kind" "NON_NULL"
                        "name" nil
                        "ofType" {"kind" "SCALAR" "name" "Int"}}
                       {"kind" "SCALAR" "name" "Int"})
                     (keyword? t)
                     (let [{:crux.graphql/keys [name]} (crux/entity db t)]
                       (cond
                         (= cardinality :crux.schema.cardinality/zero-or-more)
                         {"kind" "LIST"
                          "name" nil
                          "ofType" {"kind" "OBJECT"
                                    "name" name
                                    ;;"foo" "bar"
                                    "ofType" nil}}

                         required?
                         {"kind" "NON_NULL"
                          "name" name
                          "ofType" {"kind" "OBJECT"
                                    "name" name
                                    "ofType" nil}}

                         :else
                         {"kind" "OBJECT"
                          "name" name
                          "ofType" nil})))})
          true (conj {"isDeprecated" false}))))

    (defn- entity-to-graphql-type [db e]
      (cond
        #_(= e String)
        #_{"kind" "SCALAR" "name" "String"}
        #_(= e Integer)
        #_{"kind" "SCALAR" "name" "Int"}
        (map? e)
        (let [{:crux.graphql/keys [name]
               :crux.schema/keys [description attributes]
               t :crux.schema/type} e]
          (cond-> {}
            t (conj ["kind" "OBJECT"])
            name (conj ["name" name])
            description (conj ["description" description])
            true (conj [:attributes attributes])
            ;;            attributes (conj ["fields" (mapv #(attribute-to-graphql-field db %) (vals attributes))])
            ))
        :else (throw (ex-info "Condition not supported" {:entity e}))))

    (defrecord DbType [db]
      Type
      (lookup-field [this field-name]
        (let [attribute (some #(when (= (:crux.graphql/name %) field-name) %) (vals (:attributes this)))
              field (attribute-to-graphql-field db attribute)]

          field)))

    (defmethod print-method DbType [c w]
      (print-method (into {} (assoc c :db "(Crux db)")) w))

    (defrecord DbSchema [db]
      Schema
      (lookup-type [_ type-name]
        (map->DbType
         (into {:db db}
               (entity-to-graphql-type
                db
                (crux/entity
                 db
                 (first
                  (map
                   first
                   (crux/q
                    db
                    {:find ['?e]
                     :args [{'?tn type-name}]
                     :where [['?e :crux.graphql/name '?tn]]})))))))))

    (defmethod print-method DbSchema [c w]
      (print-method (into {} (assoc c :db "(Crux db)")) w))

    (with-open
      [node (crux/start-node {:crux.node/topology '[crux.standalone/topology]})]
      (crux/await-tx
       node
       (crux/submit-tx
        node
        (for [ent
              [{
                ;; The Crux id also acts as the name of the relation schema
                ;; represented by this entity.
                :crux.db/id :ex.type/film
                :crux.schema/type :crux.schema.type/relation
                :crux.schema/description "A James Bond Film"
                :crux.graphql/name "Film"
                :crux.schema/attributes
                {:film/name
                 {:crux.schema/description "The film's title"
                  :crux.schema/type String
                  :crux.schema/required? true
                  :crux.graphql/name "filmName"}
                 :film/box
                 {:crux.schema/type Integer
                  :crux.schema/description "How much the film made at the box office"
                  :crux.graphql/name "box"}
                 :film/cost
                 {:crux.schema/description "How much the film cost to make"
                  :crux.schema/type Integer
                  :crux.graphql/name "cost"}
                 :film/earnings
                 {:crux.schema/type String
                  :crux.schema/derived
                  ^:crux.memoize '(fn [{:film/keys [box cost]}] (- box cost))
                  :crux.graphql/name "earnings"}
                 :film/vehicle
                 {:crux.schema/type :ex.type/vehicle
                  :crux.schema/cardinality :crux.schema.cardinality/zero-or-more
                  :crux.graphql/name "vehicles"}}}

               {:crux.db/id :ex.type/vehicle
                :crux.schema/type :crux.schema.type/relation
                :crux.graphql/name "Vehicle"
                :crux.schema/attributes
                {:vehicle/brand
                 {:crux.schema/type String
                  :crux.graphql/name "brand"}
                 :vehicle/model
                 {:crux.schema/type String
                  :crux.graphql/name "model"}}}

               {:crux.db/id :ex.type/graphql-query-root
                :crux.schema/type :crux.schema.type/relation
                :crux.graphql/name "Root"
                :crux.schema/attributes
                {:all-films
                 {:crux.schema/description "All the films in the James Bond universe."
                  :crux.schema/type :ex.type/film
                  :crux.schema/cardinality :crux.schema.cardinality/zero-or-more
                  :crux.graphql/name "allFilms"}
                 :film
                 {:crux.schema/description "A particular film in the James Bond universe."
                  :crux.schema/type :ex.type/film
                  :crux.graphql/name "film"}}}]]
          [:crux.tx/put ent])))

      (crux/await-tx
       node
       (crux/submit-tx
        node
        (for [record (edn/read-string (slurp (io/resource "james-bond.edn")))]
          [:crux.tx/put record])))

      (let [db (crux/db node)
            schema
            (-> {"queryType" "Root"
                 #_"types"
                 #_(mapv #(entity-to-graphql-type db %)
                         [(crux/entity db :ex.type/graphql-query-root)
                          ;; TODO: All the rest should be discovered via graph traversal
                          (crux/entity db :ex.type/film)
                          (crux/entity db :ex.type/vehicle)
                          String
                          ])
                 "directives" []}
                validate-schema)]

        ;; Extract GraphQL types to file, just to compare
        (spit "/tmp/schema.json"
              (json/generate-string
               {:data
                {"__schema" schema}}
               {:pretty true}))

        ;; Query the schema with GraphQL execution

        (let [document
              (->
               "query GetFilms { allFilms { filmName }}"
               #_introspection-query
               graphql/parse-graphql
               graphql/validate-graphql-document)]

          ;; TODO: Introspection (Attempt to execute this query against the schema)

          (execute-request
           {:schema (map->DbSchema
                     {"queryType" "Root"
                      "directives" []
                      :db db})
            :document document
            :operation-name "GetFilms" #_"IntrospectionQuery"
            :variable-values {}
            :initial-value (crux/entity db :ex.type/graphql-query-root)
            :field-resolver
            (fn [{:keys [object-type field-name object-value argument-values] :as args}]
              (let [field (some
                           #(when (= (get % :crux.graphql/name) field-name) %)
                           (vals (:crux.schema/attributes object-value)))
                    field-type (:crux.schema/type field)
                    _ (assert (keyword? field-type))

                    e (crux/entity db field-type)

                    required-attributes
                    (for [[k v] (:crux.schema/attributes e)
                          :when (:crux.schema/required? v)]
                      k)

                    datalog
                    {:find ['?e]
                     :where (vec (for [k required-attributes]
                                   ['?e k]))}

                    results
                    (map first (crux/q db datalog))]

                results))}))))))

;; https://en.wikipedia.org/wiki/Functional_dependency
;; https://en.wikipedia.org/wiki/Armstrong%27s_axioms
;; https://en.wikipedia.org/wiki/The_Third_Manifesto
