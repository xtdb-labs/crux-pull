;; Copyright © 2020, JUXT LTD.

(ns crux.pull.graphql.schema-test
  (:require
   [cheshire.core :as json]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [clojure.test :refer [deftest is are testing run-tests successful?]]
   [crux.api :as crux]
   [crux.pull.alpha.graphql :as graphql]
   [crux.pull.eql-graphql-test :refer [introspection-query]]
   [flatland.ordered.map :refer [ordered-map]]))

(do

  (defprotocol Schema
    (resolve-type [_ object-type field-name]))

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

     (ordered-map)                      ; grouped-fields
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
          argument-definitions [] ;; (get (lookup-field object-type field-name) "args")
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
    (throw (ex-info "TODO: resolve-abstract-type" (meta #'resolve-abstract-type))))

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
    [{:keys [field-type fields result variable-values field-resolver schema]}]

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
        ;; a. If result is not a collection of values, throw a field error.
        (when-not (sequential? result)
          (throw (ex-info "Resolver must return a collection" {:field-type field-type}))
          )
        ;; b. Let innerType be the inner type of fieldType.
        (let [inner-type (get field-type "ofType")]
          ;; c. Return a list where each list item is the result of calling
          ;; CompleteValue(innerType, fields, resultItem, variableValues),
          ;; where resultItem is each item in result.

          (doall
           (for [result-item result]
             (complete-value
              {:field-type inner-type
               :fields fields
               :result result-item
               :variable-values variable-values
               :field-resolver field-resolver
               :schema schema})))))

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
          :field-resolver field-resolver
          :schema schema}))))

  (defn
    ^{:crux.graphql.spec-ref/version "June2018"
      :crux.graphql.spec-ref/section "6.4"
      :crux.graphql.spec-ref/algorithm "ExecuteField"}
    execute-field
    [{:keys [object-type object-value field-type fields variable-values field-resolver schema]}]

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

      ;; 5. Return the result of CompleteValue(…).
      (complete-value
       {:field-type field-type
        :fields fields
        :result resolved-value
        :variable-values variable-values
        :field-resolver field-resolver
        :schema schema})))

  (defn
    ^{:crux.graphql.spec-ref/version "June2018"
      :crux.graphql.spec-ref/section "6.3"
      :crux.graphql.spec-ref/algorithm "ExecuteSelectionSet"}
    execute-selection-set-normally
    "Return a map with :data and :errors."
    [{:keys [selection-set object-type object-value variable-values field-resolver schema]}]

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

         ;; a. Let fieldName be the name of the first entry in fields. Note:
         ;; This value is unaffected if an alias is used.
         (let [field-name (:name (first fields))
               ;; b. Let fieldType be the return type defined for the field fieldName of objectType.
               field-type (resolve-type schema object-type field-name)
               ]

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
                     :field-resolver field-resolver
                     :schema schema})]
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
          query-type (resolve-type
                      schema
                      {:crux.schema/attributes
                       {:root {:crux.graphql/name "Root"
                               :crux.schema/type :ex.type/graphql-query-root}}}
                      query-type-name)]

      #_(println "query-type" (pr-str query-type))

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
          :schema schema
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

        (throw (ex-info "No operation type on operation" {:operation operation})))))

  (defrecord DbSchema [db]
    Schema

    (resolve-type [this object-type field-name]
      (assert (:crux.schema/attributes object-type) "Not ours!")
      (let [result
            (let [attribute (some
                             #(when (= (get % :crux.graphql/name) field-name) %)
                             (vals (:crux.schema/attributes object-type)))
                  {:crux.schema/keys [cardinality required? description]
                   type-ref :crux.schema/type} attribute]
              (cond
                (= type-ref String)
                {"kind" "SCALAR"
                 "name" "String"
                 "typeOf" nil}

                (= type-ref Integer)
                {"kind" "SCALAR"
                 "name" "Int"
                 "typeOf" nil}

                (keyword? type-ref)
                (let [{:crux.graphql/keys [name]
                       :crux.schema/keys [attributes]
                       t :crux.schema/type}
                      (crux/entity db type-ref)]
                  ;;(println "description" (pr-str description))
                  ;;(println "cardinality" (pr-str cardinality))
                  ;;(println "required?" (pr-str required?))
                  (cond
                    (= cardinality :crux.schema.cardinality/many)
                    {"kind" "LIST"
                     "name" nil
                     "ofType" {"kind" "OBJECT"
                               "name" name
                               "ofType" nil
                               :crux.schema/attributes attributes}}

                    required?
                    {"kind" "NON_NULL"
                     "name" name
                     "ofType" {"kind" "OBJECT"
                               "name" name
                               "ofType" nil
                               :crux.schema/attributes attributes}}

                    :else
                    {"kind" "OBJECT"
                     "name" name
                     "ofType" nil
                     :crux.schema/attributes attributes}))

                :else
                (throw
                 (ex-info
                  "TODO: resolve type"
                  {:object-type object-type
                   :field-name field-name
                   :type-ref type-ref
                   :type-ref-type (type type-ref)}))))]
        result)))

  (defmethod print-method DbSchema [c w]
    (print-method (into {} (assoc c :schema "(schema)")) w))

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
                :crux.schema/cardinality :crux.schema.cardinality/one
                :crux.schema/required? true
                :crux.graphql/name "filmName"}
               :film/box
               {:crux.schema/type Integer
                :crux.schema/description "How much the film made at the box office"
                :crux.schema/cardinality :crux.schema.cardinality/one
                :crux.schema/required? false
                :crux.graphql/name "box"}
               :film/cost
               {:crux.schema/type Integer
                :crux.schema/description "How much the film cost to make"
                :crux.schema/cardinality :crux.schema.cardinality/one
                :crux.schema/required? false
                :crux.graphql/name "cost"}
               :film/earnings
               {:crux.schema/type String
                :crux.schema/cardinality :crux.schema.cardinality/one
                :crux.schema/dependencies #{:film/box :film/cost}
                :crux.schema/derived
                ^:crux.memoize '(fn [{:film/keys [box cost]}] (- box cost))
                :crux.graphql/name "earnings"}
               :film/vehicles
               {:crux.schema/type :ex.type/vehicle
                :crux.schema/join :film/vehicles
                :crux.schema/cardinality :crux.schema.cardinality/many
                :crux.graphql/name "vehicles"}
               :film/director
               {:crux.schema/type :ex.type/director
                :crux.schema/cardinality :crux.schema.cardinality/one
                :crux.schema/required? true
                :crux.graphql/name "director"}}}

             {:crux.db/id :ex.type/director
              :crux.schema/type :crux.schema.type/relation
              :crux.graphql/name "Director"
              :crux.schema/attributes
              {:person/name
               {:crux.schema/type String
                :crux.schema/cardinality :crux.schema.cardinality/one
                :crux.schema/required? true
                :crux.graphql/name "name"}}}

             {:crux.db/id :ex.type/vehicle
              :crux.schema/type :crux.schema.type/relation
              :crux.graphql/name "Vehicle"
              :crux.schema/attributes
              {:vehicle/brand
               {:crux.schema/type String
                :crux.schema/cardinality :crux.schema.cardinality/one
                :crux.schema/required? true
                :crux.graphql/name "brand"}
               :vehicle/model
               {:crux.schema/type String
                :crux.schema/cardinality :crux.schema.cardinality/one
                :crux.schema/required? true
                :crux.graphql/name "model"}}}

             {:crux.db/id :ex.type/graphql-query-root
              :crux.schema/type :crux.schema.type/relation
              :crux.graphql/name "Root"
              :crux.schema/attributes
              {:all-films
               {:crux.schema/description "All the films in the James Bond universe."
                :crux.schema/type :ex.type/film
                :crux.schema/cardinality :crux.schema.cardinality/many
                :crux.graphql/name "allFilms"}
               :film
               {:crux.schema/description "A particular film in the James Bond universe."
                :crux.schema/type :ex.type/film
                :crux.schema/cardinality :crux.schema.cardinality/one
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
             "query GetFilms { allFilms { filmName box vehicles { brand model }}}"
             #_introspection-query
             graphql/parse-graphql
             graphql/validate-graphql-document)

            schema (map->DbSchema
                    {"queryType" "Root"
                     "directives" []
                     :db db})]

        ;; TODO: Introspection (Attempt to execute this query against the schema)

        ;; Schema is complected, separate into schema and 'type-resolver'

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
            (println "resolving field" field-name "against object-value of" object-value)
            (let [[attr-k attr]
                  (some
                   #(when (= (get (second %) :crux.graphql/name) field-name) %)
                   (:crux.schema/attributes object-type))

                  field-type (:crux.schema/type attr)]

              (cond

                ;; A :crux.schema/type of a keyword, absolutely implies this is
                ;; a reference to another relation. The cardinality is implied
                ;; to be :crux.schema.cardinality/zero-or-more, but we should
                ;; think about whether cardinality needs to be explicit in this
                ;; case, and what other cardinalities would mean.
                (keyword? field-type)
                ;; Check type is list first?
                (let [relation (crux/entity db field-type)

                      required-attributes
                      (for [[k v] (:crux.schema/attributes relation)
                            :when (:crux.schema/required? v)]
                        k)

                      datalog
                      {:find ['?e]
                       :where (vec
                               (cond->
                                   (for [k required-attributes]
                                     ['?e k])
                                   (:crux.schema/join attr)
                                   (conj [(:crux.db/id object-value) (:crux.schema/join attr) '?e])))}]

                  (println "DATALOG")
                  (pprint datalog)

                  (for [ref (map first (crux/q db datalog))]
                    (crux/entity db ref)))

                (#{String Integer} field-type)
                ;; What if the :crux.schema/type of the field is different?
                (get object-value attr-k)

                :else
                (throw
                 (ex-info
                  "TODO: resolve field"
                  {:attr-k attr-k
                   :field-name field-name
                   :object-value object-value
                   :object-type object-type
                   :attr attr
                   :field-type field-type})))))})))))

;; https://en.wikipedia.org/wiki/Functional_dependency
;; https://en.wikipedia.org/wiki/Armstrong%27s_axioms
;; https://en.wikipedia.org/wiki/The_Third_Manifesto
