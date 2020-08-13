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
   [flatland.ordered.map :refer [ordered-map]]
   [clojure.set :as set]))

(do
  (defprotocol Schema
    (resolve-type [_ object-type field-name]))

  (defn
    ^{:crux.graphql.spec-ref/version "June2018"
      :crux.graphql.spec-ref/section "6.3.2"
      :crux.graphql.spec-ref/algorithm "CollectFields"}
    collect-fields
    [{:keys [object-type selection-set variable-values visited-fragments document]
      ;; 1. If visitedFragments if not provided, initialize it to the empty
      ;; set.
      :or {visited-fragments #{}}}]

    (assert document)

    (reduce
     (fn [grouped-fields [selection-type selection]]
       (case selection-type
         ;; c. If selection is a Field:
         :field
         (let [response-key
               ;; i. Let responseKey be the response key of selection (the alias
               ;; if defined, otherwise the field name).

               ;; TODO: The response-key will be the alias, if it exists
               (:name selection)]
           (update
            grouped-fields
            ;; ii. Let groupForResponseKey be the list in groupedFields for responseKey;
            response-key

            ;; Append selection to the groupForResponseKey.
            (-> conj
                ;; ii (cont). …if no such list exists, create it as an empty
                ;; list.
                (fnil []))
            selection))

         :fragment-spread
         ;; d. If selection is a FragmentSpread:
         ;; i. Let fragmentSpreadName be the name of selection.
         (let [fragment-spread-name (:fragment-name selection)]
           ;; ii. If fragmentSpreadName is in visitedFragments, continue with
           ;; the next selection in selectionSet.
           (if (contains? visited-fragments fragment-spread-name)
             grouped-fields

             (let [ ;; iii. Add fragmentSpreadName to visitedFragments.
                   visited-fragments (conj visited-fragments fragment-spread-name)
                   ;; iv. Let fragment be the Fragment in the current Document
                   ;; whose name is fragmentSpreadName.
                   fragment (some
                             #(when (= (:fragment-name %) fragment-spread-name)
                                %) document)]
               ;; v. If no such fragment exists, continue with the next
               ;; selection in selectionSet.
               (if-not fragment
                 grouped-fields

                 ;; vi. Let fragmentType be the type condition on fragment.
                 (let [fragment-type (:named-type fragment)]

                   ;; vii. If DoesFragmentTypeApply(objectType, fragmentType) is
                   ;; false, continue with the next selection in selectionSet. (TODO)

                   (let [
                         ;; viii. Let fragmentSelectionSet be the top‐level selection
                         ;; set of fragment.
                         fragment-selection-set (:selection-set fragment)
                         ;; ix. Let fragmentGroupedFieldSet be the result of calling
                         ;; CollectFields(objectType, fragmentSelectionSet,
                         ;; visitedFragments).
                         fragment-group-field-set
                         (collect-fields
                          {:object-type object-type
                           :selection-set fragment-selection-set
                           :variable-values variable-values
                           :visited-fragments visited-fragments
                           :document document})]

                     (reduce
                      (fn [grouped-fields [response-key fragment-group]]
                        (update grouped-fields response-key (fnil concat (list)) fragment-group))
                      grouped-fields
                      fragment-group-field-set)

                     #_(throw
                        (ex-info
                         "TODO: fragment-spread"
                         {:selection selection
                          :fragment-spread-name fragment-spread-name
                          :visited-fragments visited-fragments
                          :fragment fragment
                          :fragment-group-field-set fragment-group-field-set
                          ;;:document document
                          }))

                     )

                   )

                 )
               )))

         :inline-fragment
         (throw (ex-info "TODO: inline-fragment" {:selection selection}))))

     ;; 2. Initialize groupedFields to an empty ordered map of lists.
     (ordered-map)
     ;; 3. For each selection in selectionSet:
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
          argument-definitions (some #(when (= (get % "name") field-name) (get % "args")) (get object-type "fields"))]

      ;; 5. For each argumentDefinition in argumentDefinitions:
      (reduce
       (fn [acc argument-definition]

         (let [ ;; a. Let argumentName be the name of argumentDefinition.
               argument-name (get argument-definition "name")
               ;; b. Let argumentType be the expected type of argumentDefinition.
               argument-type (get argument-definition "type")
               ;; c. Let defaultValue be the default value for argumentDefinition.
               default-value (find argument-definition "defaultValue")
               ;; d. Let hasValue be true if argumentValues provides
               ;; a value for the name argumentName.
               has-value (find argument-values argument-name)
               ;; e. Let argumentValue be the value provided in argumentValues for the name argumentName.
               argument-value (second has-value)
               ;; f. If argumentValue is a Variable: (TODO)
               ;; g. Otherwise, let value be argumentValue.
               value argument-value]

           (cond
             ;; h. If hasValue is not true and defaultValue exists (including null):
             (and (not has-value) default-value)
             ;;   i. Add an entry to coercedValues named argumentName
             ;;   with the value defaultValue.
             (conj acc [argument-name (second default-value)])

             ;; i. Otherwise if argumentType is a Non‐Nullable type,
             ;; and either hasValue is not true or value is null,
             ;; throw a field error.
             (and (= (get argument-type "kind") "NON_NULL")
                  (or (not has-value)
                      (nil? (second has-value))))
             (throw (ex-info "Field error, argument type is wrapped as non-null, but no argument value given" {}))

             ;; j. Otherwise if hasValue is true:
             has-value
             (cond
               ;; i. If value is null:
               (nil? (second has-value))
               ;; 1. Add an entry to coercedValues named argumentName with the value null.
               (conj acc [argument-name nil])
               ;; ii. Otherwise, if argumentValue is a Variable: (TODO)

               :else
               ;; TODO: apply coercion rules, for now just set it to the value
               (let [coerced-value value]
                 (conj acc [argument-name value])))

             :else acc)))

       coerced-values
       argument-definitions)))

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
    [{:keys [object-type object-value field-name argument-values field-resolver] :as args}]
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
    [{:keys [field-type fields result variable-values field-resolver schema document]}]

    (assert field-type)
    (assert schema)
    (assert document)

    (cond
      ;; 1. If the fieldType is a Non‐Null type:
      (= (get field-type "kind") "NON_NULL")
      ;; a. Let innerType be the inner type of fieldType.
      (let [inner-type (get field-type "ofType")
            _ (assert inner-type (format "Field type %s is NON_NULL but doesn't have a non-nil ofType" (pr-str field-type)))
            ;; b. Let completedResult be the result of calling
            ;; CompleteValue(…).
            completed-result
            (try
              (complete-value
               {:field-type inner-type
                :fields fields
                :result result
                :variable-values variable-values
                :field-resolver field-resolver
                :schema schema
                :document document})
              (catch Throwable e
                (throw
                 (ex-info
                  "Error on complete-value"
                  {:field-type field-type
                   :inner-type inner-type}
                  e))
                ))]
        ;; c. If completedResult is null, throw a field error.
        (when (nil? completed-result)
          (throw (ex-info
                  "Field error, NON_NULL type returned nil value for inner type"
                  {:inner-type inner-type
                   :result result})))
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
               :schema schema
               :document document})))))

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
          :schema schema
          :document document}))))

  (defn
    ^{:crux.graphql.spec-ref/version "June2018"
      :crux.graphql.spec-ref/section "6.4"
      :crux.graphql.spec-ref/algorithm "ExecuteField"}
    execute-field
    [{:keys [object-type object-value field-type fields variable-values field-resolver schema document]}]
    (assert schema)
    (assert document)

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
        :schema schema
        :document document})))

  (defn
    ^{:crux.graphql.spec-ref/version "June2018"
      :crux.graphql.spec-ref/section "6.3"
      :crux.graphql.spec-ref/algorithm "ExecuteSelectionSet"}
    execute-selection-set-normally
    "Return a map with :data and :errors."
    [{:keys [selection-set object-type object-value variable-values field-resolver schema document]}]

    (assert schema)
    (assert document)

    ;; 1. Let groupedFieldSet be the result of CollectFields
    (let [grouped-field-set
          (collect-fields
           {:object-type object-type
            :selection-set selection-set
            :variable-values variable-values
            :document document})

          ;; 2. Initialize resultMap to an empty ordered map.
          result-map (ordered-map)]

      ;; 3. For each groupedFieldSet as responseKey and fields:
      (reduce
       (fn [result-map [response-key fields]]

         ;; a. Let fieldName be the name of the first entry in fields. Note:
         ;; This value is unaffected if an alias is used.
         (let [field-name (:name (first fields))
               ;; b. Let fieldType be the return type defined for the field fieldName of objectType.
               field-type (resolve-type schema object-type field-name)]

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
                     :schema schema
                     :document document})]
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
    [{:keys [query schema variable-values initial-value field-resolver document]}]
    (assert schema)
    (assert document)

    ;; 1. Let queryType be the root Query type in schema.
    (let [query-type-name (get schema "queryType")
          query-type (resolve-type
                      schema
                      {:crux.schema/entity
                       {:crux.schema/attributes
                        {:root {:crux.graphql/name "Root"
                                :crux.schema/type :ex.type/graphql-query-root}}}}
                      query-type-name)]

      ;; 2. Assert: queryType is an Object type.
      (when-not (= (get query-type "kind") "OBJECT")
        (throw (ex-info
                "Query type must be an OBJECT"
                (into
                 {:query-type query-type
                  :query-type-name query-type-name
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
          :field-resolver field-resolver
          :document document}))))

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
          :field-resolver field-resolver
          :document document})

        ;; 4. Otherwise if operation is a mutation operation:
        ;;   a. Return ExecuteMutation(operation, schema, coercedVariableValues, initialValue).

        ;; TODO

        ;; 5. Otherwise if operation is a subscription operation:
        ;;   a. Return Subscribe(operation, schema, coercedVariableValues, initialValue).

        ;; TODO

        (throw (ex-info "No operation type on operation" {:operation operation})))))

  (defn to-graphql-type [db t]
    (cond
      (= t String)
      {"kind" "SCALAR"
       "name" "String"}
      (= t Integer)
      {"kind" "SCALAR"
       "name" "Int"}
      (keyword? t)
      (let [e (crux/entity db t)
            {:crux.graphql/keys [name]
             :crux.schema/keys [attributes]}
            e]
        {"kind" "OBJECT"
         ;; TODO: description
         "name" name
         "fields"
         (mapv
          (fn [[attr-n {:crux.schema/keys [description arguments type]
                        :crux.graphql/keys [name]}]]
            (cond-> {"name" name}
              description (conj ["description" description])
              true                      ; args is mandatory
              (conj ["args"
                     (mapv
                      (fn [[arg-k {n :crux.graphql/name
                                   desc :crux.schema/description
                                   t :crux.schema/type
                                   :as arg}]]
                        (assert n (format "InputValue must have a name: %s" (pr-str arg)))
                        (assert t (format "InputValue must have a type: %s" (pr-str arg)))
                        (cond->
                            {"name" n}
                            desc (conj ["description" desc])
                            true (conj ["type" (to-graphql-type db t)])))
                      arguments)])
              true
              (conj ["type" (to-graphql-type db type)])
              true
              (conj ["isDeprecated" false])))
          attributes)

         :crux.schema/entity e})

      :else (throw (ex-info "Cannot convert to GraphQL type" {:t t}))))

  (defrecord DbSchema [db]
    Schema

    (resolve-type [this object-type field-name]
      (printf "Resolving type, field-name is '%s'\n" field-name)
      (cond
        (= field-name "__schema")
        {"kind" "OBJECT"
         "name" "__Schema"
         "description" "A GraphQL Schema defines the capabilities of a GraphQL server. It exposes all available types and directives on the server, as well as the entry points for query, mutation, and subscription operations."
         "fields" [{"name" "types"
                    "description" "A list of all types supported by this server."
                    "args" []
                    "type" {"kind" "NON_NULL"
                            "ofType" {"kind" "LIST"
                                      "ofType" {"kind" "NON_NULL"
                                                "ofType" {"kind" "OBJECT"
                                                          "name" "__Type"}}}}
                    "isDeprecated" false}

                   {"name" "queryType"
                    "description" "The type that query operations will be rooted at."
                    "args" []
                    "type" {"kind" "NON_NULL"
                            "name" nil
                            "ofType" {"kind" "OBJECT"
                                      "name" "__Type"}}
                    "isDeprecated" false}

                   {"name" "mutationType"
                    "description" "If this server supports mutation, the type that mutation operations will be rooted at."
                    "args" []
                    "type" {"kind" "OBJECT"
                            "name" "__Type"}
                    "isDeprecated" false}

                   {"name" "subscriptionType"
                    "description" "If this server support subscription, the type that subscription operations will be rooted at."
                    "args" []
                    "type" {"kind" "OBJECT"
                            "name" "__Type"}
                    "isDeprecated" false}

                   {"name" "directives"
                    "description" "A list of all directives supported by this server."
                    "args" []
                    "type" {"kind" "NON_NULL"
                            "ofType" {"kind" "LIST"
                                      "ofType" {"kind" "NON_NULL"
                                                "ofType" {"kind" "OBJECT"
                                                          "name" "__Directive"}}}}
                    "isDeprecated" false}]}

        (= (get object-type "name") "__Schema")
        (if-let [type (some #(when (= (get % "name") field-name) (get % "type")) (get object-type "fields"))]
          type
          (throw
           (ex-info
            "Resolve schema type"
            {:object-type object-type
             :field-name field-name})))

        (= (get object-type "name") "__Type")
        (case field-name
          "kind" {"kind" "ENUM"
                  "enumValues" []}

          "name" {"kind" "SCALAR"
                  "name" "String"}

          "description" {"kind" "SCALAR"
                         "name" "String"}

          "fields" {"kind" "LIST"
                    "ofType" {"kind" "NON_NULL"
                              "ofType" {"kind" "OBJECT"
                                        "name" "__Field"}}}
          "interfaces" {"kind" "LIST"
                        "ofType" {"kind" "NON_NULL"
                                  "ofType" {"kind" "OBJECT"
                                            "name" "__Type"}}}
          "possibleTypes" {"kind" "LIST"
                           "ofType" {"kind" "NON_NULL"
                                     "ofType" {"kind" "OBJECT"
                                               "name" "__Type"}}}

          "enumValues" {"kind" "LIST"
                        "ofType" {"kind" "NON_NULL"
                                  "ofType" {"kind" "OBJECT"
                                            "name" "__EnumValue"}}}

          "inputFields" {"kind" "LIST"
                         "ofType" {"kind" "NON_NULL"
                                   "ofType" {"kind" "OBJECT"
                                             "name" "__InputValue"}}}

          "ofType" {"kind" "OBJECT"
                    "name" "__Type"}

          (throw (ex-info "No case" {:field-name field-name})))

        (= (get object-type "name") "__Field")
        (case field-name
          "name" {"kind" "NON_NULL"
                  "ofType" {"kind" "SCALAR"
                            "name" "String"}}

          "description" {"kind" "SCALAR"
                         "name" "String"}

          "args" {"kind" "NON_NULL"
                  "ofType" {"kind" "LIST"
                            "ofType" {"kind" "NON_NULL"
                                      "ofType" {"kind" "OBJECT"
                                                "name" "__InputValue"}}}}
          "type" {"kind" "NON_NULL"
                  "ofType" {"kind" "OBJECT"
                            "name" "__Type"}}

          "isDeprecated" {"kind" "NON_NULL"
                          "ofType" {"kind" "SCALAR"
                                    "name" "Boolean"}}

          "deprecationReason" {"kind" "SCALAR"
                               "name" "String"})


        (= (get object-type "name") "__InputValue")
        (case field-name
          "name" {"kind" "NON_NULL"
                  "ofType" {"kind" "SCALAR"
                            "name" "String"}}

          "description" {"kind" "SCALAR"
                         "name" "String"}

          "type" {"kind" "NON_NULL"
                  "ofType" {"kind" "OBJECT"
                            "name" "__Type"}}

          "defaultValue" {"kind" "SCALAR"
                          "name" "String"})

        (= (get object-type "name") "__EnumValue")
        (throw (ex-info "TODO: __EnumValue" {}))

        (= (get object-type "name") "__Directive")
        (throw (ex-info "TODO: __Directive" {}))

        ;; Crux backed
        (:crux.schema/entity object-type)
        (let [attribute
              (some
               #(when (= (get % :crux.graphql/name) field-name) %)
               (vals (get-in object-type [:crux.schema/entity :crux.schema/attributes])))

              {:crux.schema/keys [cardinality required? description]
               type-ref :crux.schema/type}
              attribute

              graphql-object-type (to-graphql-type db type-ref)]
          (cond
            (= cardinality :crux.schema.cardinality/many)
            {"kind" "LIST"
             "ofType" graphql-object-type}

            required?
            {"kind" "NON_NULL"
             "ofType" graphql-object-type}

            :else graphql-object-type))

        :else
        (throw
         (ex-info
          (format "TODO: resolve unknown type: %s" field-name)
          {:object-type object-type
           :field-name field-name})))))

  (defmethod print-method DbSchema [c w]
    (print-method (into {} (assoc c :schema "(schema)")) w))

  ;; TODO: This fn appears to only visit types of attributes, not arg types in relations
  (defn visit-types [db tref visited-set]
    (cond
      (keyword? tref)
      (let [e (crux/entity db tref)]
        (cons tref
              (let [visits (set/difference
                            (set (map (fn [[_ v]] (:crux.schema/type v)) (:crux.schema/attributes e)))
                            visited-set)
                    new-visited-set (set/union visited-set visits)]
                (mapcat (fn [t] (visit-types db t new-visited-set)) visits))))
      :else
      [tref]))

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
               :film/year
               {:crux.schema/description "The film's year"
                :crux.schema/type String
                :crux.schema/cardinality :crux.schema.cardinality/one
                :crux.schema/required? false
                :crux.graphql/name "year"}
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
                :crux.schema/required? false
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
                ;; The type is the relation was are targetting.
                :crux.schema/type :ex.type/film
                :crux.schema/cardinality :crux.schema.cardinality/one

                ;; GraphQL fields are the analog of Crux attributes. Fields have
                ;; arguments, so makes sense to declare them here, at the
                ;; attribute value level.
                :crux.schema/arguments
                {:id
                 {:crux.schema/join :crux.db/id
                  :crux.graphql/name "id"
                  :crux.schema/type String}

                 :name
                 {:crux.schema/join :film/name
                  :crux.schema/comparator :crux.schema.comparator/equals
                  :crux.graphql/name "name"
                  :crux.schema/type String}

                 :year
                 {:crux.schema/join :film/year
                  :crux.schema/comparator :crux.schema.comparator/equals
                  :crux.graphql/name "madeIn"
                  :crux.schema/type String}

                 :cost-more-than
                 {:crux.schema/join :film/cost
                  :crux.schema/description "A comparison query on cost"
                  :crux.schema/comparator :crux.schema.comparator/greater-than
                  :crux.graphql/name "costsMore"
                  :crux.schema/type Integer}}

                :crux.graphql/name "film"}}}]]
        [:crux.tx/put ent])))

    (crux/await-tx
     node
     (crux/submit-tx
      node
      (for [record
            (edn/read-string
             (slurp
              (io/resource "james-bond.edn")))]
        [:crux.tx/put record])))



    (let [db (crux/db node)
          schema (map->DbSchema
                  {"queryType" "Root"
                   :db db})]

      ;; Query the schema with GraphQL execution
      ;;


      (let [document
            (->
             #_"query { allFilms { filmName box year vehicles { brand model }}}"
             #_"query { film(name:\"For Your Eyes Only\") { filmName year }}"
             #_"query { film(madeIn:\"1963\") { filmName }}"
             introspection-query

             graphql/parse-graphql
             graphql/validate-graphql-document)

            ]

        ;; TODO: Introspection (Attempt to execute this query against the schema)

        ;; Schema is complected, separate into schema and 'type-resolver'

        (execute-request
         {:schema (map->DbSchema
                   {"queryType" "Root"
                    :db db})
          :document document
          ;; TODO: Where was the original code written to determine the query to
          ;; run? Also, see spec advice on this.
          :operation-name "IntrospectionQuery"
          :variable-values {}
          :initial-value (crux/entity db :ex.type/graphql-query-root)

          :field-resolver
          (fn [{:keys [object-type field-name object-value argument-values] :as args}]

            (cond
              (= field-name "__schema")
              {"types" (mapv #(to-graphql-type db %) (visit-types db :ex.type/graphql-query-root #{}))
               ;; possible types
               "queryType" {"kind" "OBJECT"
                            "name" "Root"}
               "mutationType" nil
               "subscriptionType" nil
               "directives" []}

              (= field-name "__type")
              (throw (ex-info "TODO: __type" {}))

              (:crux.schema/entity object-type)
              (let [[attr-k attr]
                    (some
                     #(when (= (get (second %) :crux.graphql/name) field-name) %)
                     (get-in object-type [:crux.schema/entity :crux.schema/attributes]))

                    field-type (:crux.schema/type attr)]

                ;; For each arg in :crux.schema/arguments, go through with argument-values

                (cond

                  ;; A :crux.schema/type of a keyword, absolutely implies this is
                  ;; a reference to another relation. The cardinality is implied
                  ;; to be :crux.schema.cardinality/zero-or-more, but we should
                  ;; think about whether cardinality needs to be explicit in this
                  ;; case, and what other cardinalities would mean.
                  (keyword? field-type)
                  ;; Check type is list first?
                  (let [relation (crux/entity db field-type)

                        relation-required-attributes
                        (for [[k v] (:crux.schema/attributes relation)
                              :when (:crux.schema/required? v)]
                          k)

                        datalog
                        {:find ['?e]
                         :where (vec
                                 (cond->
                                     (for [k relation-required-attributes]
                                       ['?e k])

                                   (:crux.schema/join attr)
                                   (conj [(:crux.db/id object-value) (:crux.schema/join attr) '?e])

                                   (:crux.schema/arguments attr)
                                   (concat
                                    (keep (fn [[arg-k arg-v]]
                                            (when-let [[_ v] (find argument-values (:crux.graphql/name arg-v))]
                                              ['?e (:crux.schema/join arg-v) v]))
                                          (:crux.schema/arguments attr)))))}]

                    (for [ref (map first (crux/q db datalog))]
                      (crux/entity db ref)))

                  (#{String Integer} field-type)
                  ;; What if the :crux.schema/type of the field is different?
                  (get object-value attr-k)

                  :else
                  (throw
                   (ex-info
                    "TODO: resolve entity field"
                    {:attr-k attr-k
                     :field-name field-name
                     :object-value object-value
                     :object-type object-type
                     :attr attr
                     :field-type field-type}))))


              :else
              (if-let [[_ child] (find object-value field-name)]
                child

                nil
                #_(pprint
                   (ex-info
                    "TODO: resolve field"
                    {:field-name field-name
                     :object-value object-value
                     :object-type object-type
                     })))
              ))})))))

;; https://en.wikipedia.org/wiki/Functional_dependency
;; https://en.wikipedia.org/wiki/Armstrong%27s_axioms
;; https://en.wikipedia.org/wiki/The_Third_Manifesto
