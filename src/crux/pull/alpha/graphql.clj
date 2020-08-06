;; Copyright © 2020, JUXT LTD.

(ns crux.pull.alpha.graphql
  (:require
   [clojure.string :as str]
   [juxt.reap.alpha.graphql :as reap-graphql]
   [juxt.reap.alpha.graphql.util :as reap-graphql-util]
   [juxt.reap.alpha.api :as reap]
   [edn-query-language.core :as eql]
   [crux.pull.alpha.eql :as e])
  (:import
   (crux.pull.alpha.eql Resolver)))

(defn graphql-resolver [types]
  (reify Resolver
    (lookup [_ ctx ast opts]
      (let [delegate (:lookup (:meta ast))]
        (cond
          (= (:key ast) :__schema)
          {:queryType {:name "Root"}
           :mutationType nil
           :subscriptionType nil
           :types types}

          ;; If delegate, call it!
          delegate
          (delegate ctx ast opts)

          :else
          (case (:type ast)
            :join
            (get ctx (:key ast))
            :prop (get ctx (:key ast))))))))

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

(defn graphql-operation
    "Extract the GraphQL operation from the parsed document, dereferencing any
  fragment references."
    ([doc]
     (when-let [op
                (let [ops (filter #(= (:operation-type %) "query") (operations doc))]
                  (if (= (count ops) 1)
                    (first ops)
                    (graphql-operation doc nil)))]
       (reap-graphql-util/deref-fragments op doc)))
    ([doc op-name]
     (when-let [op (some #(when (= (:name %) op-name) %) doc)]
       (reap-graphql-util/deref-fragments op doc))))

(defn eql-keyword [s]
    (when s
      (let [[ns n z] (str/split s #"__")]
        (when z (throw (ex-info "Can only have one double-underscore in a name" {})))
        (cond
          ;; We want 'reserved' GraphQL keywords such as __schema to result in
          ;; :__schema
          (and ns (str/blank? ns))
          (keyword (str "__" n))
          ;; Input contains a __ which we convert to a namespaced keyword
          n
          (keyword
           (str/replace ns "_" ".")
           (str/replace n #"(?<=.)_" "."))
          ;; Input doesn't contain __, so convert to a non-namespaced keyword
          :else
          ;; TODO: Convert camel casing to kebab case
          (keyword (str/replace ns #"(?<=.)_" "."))))))

(defn selection-to-eql-term [[_ {:keys [name arguments directives selection-set]}]]
    (let [add-arguments (fn [x] (if (not-empty arguments) (list x arguments) x))]
      (if selection-set
        {(add-arguments (eql-keyword name)) (mapv selection-to-eql-term selection-set)}
        (add-arguments (eql-keyword name)))))

(defn graphql-query-to-eql
    ([^String graphql]
     (graphql-query-to-eql graphql nil))
    ([^String graphql ^String op-name]
     (let [doc (parse-graphql graphql)
           _ (validate-graphql-document doc)

           op (graphql-operation doc op-name)
           _ (when-not op
               (throw
                (ex-info
                 "Operation not found in doc"
                 {:operation-name op-name
                  :doc doc})))

           _ (when-not (or (nil? (:operation-type op))
                           (= (:operation-type op) "query"))
               (throw
                (ex-info
                 "Operation must be a query"
                 {:operation op
                  :operation-type (:operation-type op)})))

           selection-set (:selection-set op)
           eql (mapv selection-to-eql-term selection-set)]
       eql)))


(def TypeRef
  [:kind
   :name
   {:ofType
    ^:single
    [:kind :name
     ^:single
     {:ofType
      [:kind
       :name
       ^:single
       {:ofType
        [:kind
         :name
         ^:single
         {:ofType
          [:kind
           :name
           ^:single
           {:ofType
            [:kind
             :name
             ^:single
             {:ofType
              [:kind
               :name
               ^:single
               {:ofType
                [:kind
                 :name
                 ^:single
                 {:ofType
                  [:kind
                   :name
                   ^:single
                   {:ofType
                    [:kind
                     :name]}]}]}]}]}]}]}]}]}])

(def InputValue
  [:name
   :description
   ^:single {:type TypeRef}
   :defaultValue])

(def
  EnumValue
  [:name :description :isDeprecated :deprecationReason])

(def
  Field
  [:name
   :description
   {:args InputValue}
   ^:single {:type TypeRef}
   :isDeprecated
   :deprecationReason])

(def
  Type
  [:kind
   :name
   :description
   {:fields Field}
   {:interfaces TypeRef}
   {:possibleTypes TypeRef}
   {:enumValues EnumValue}
   {:inputFields InputValue}
   ^:single
   {:ofType TypeRef}])

(def Directive
  [:name :description :locations {:args InputValue}])

(defn add-introspection [schema]
  (->
   schema
   (update
    :children
    conj
    (eql/expr->ast
     ^:single
     {:__schema
      [{:types Type}
       ^:single {:queryType TypeRef}
       ^:single {:mutationType TypeRef}
       ^:single {:subscriptionType TypeRef}
       {:directives Directive}]}))))


(defn graphql-name [k]
  (when k
    (if (namespace k)
      (format
       "%s__%s"
       (str/replace (namespace k) "." "_")
       (str/replace (name k) "." "_"))
      (str/replace (name k) "." "_"))))

;;(graphql-name :film/_vehicles)
;;(name (eql-keyword "film___vehicles"))

(declare eql-ast-node-to-graphql-type)

(defn eql-ast-node-to-graphql-field
  "Each field has a kind, name, description"
  [node]
  (let [description (get-in node [:meta :crux.graphql/description])]
    (cond-> {:name
             (or
              (get-in node [:meta :crux.graphql/name])
              (graphql-name (:dispatch-key node)))}

      description
      (conj [:description description])

      ;; Required array of InputValue!
      ;; name, description, type, defaultValue
      ;; Any parameters? Check EDN list
      true (conj [:args
                  (mapv
                   (fn [[n m]]
                     (let [desc (get m :crux.graphql/description)
                           type (get m :crux.graphql/type)
                           default-value (get m :crux.graphql/default-value)]
                       (cond-> {:name n}
                         desc (conj [:description desc])
                         type (conj [:type type])
                         default-value (conj [:defaultValue default-value]))))
                   (get-in node [:meta :params]))])

      ;; Types are required. We define 'inline', based on the set of available
      ;; children. There is an important question about whether types can be
      ;; embedded in schema queries, or whether they must be extracted out to
      ;; the top level and referenced where used. The SWAPI example does the
      ;; latter, and we're not yet sure whether GraphiQL requires this or
      ;; not. The only way to be sure is to work on the basis that inline
      ;; types aren't possible (although they appear to be in the spec), get a
      ;; demo working and testable against GraphiQL, and then later refactor
      ;; to try out the change to inline types. Note that in EQL, there are NO
      ;; types to worry about.
      true
      (conj [:type (eql-ast-node-to-graphql-type node)])

      true (conj [:isDeprecated false])
      false (conj [:deprecationReason nil]))))

(defn eql-ast-node-to-graphql-type
  "A type in GraphQL is required to have a kind:

  SCALAR, OBJECT, INTERFACE, UNION, ENUM, INPUT_OBJECT, LIST, NON_NULL.

  See https://spec.graphql.org/June2018/#sec-Schema-Introspection for full
  details."
  [node]
  (let [kind (case (:type node)
               :root "OBJECT"
               ;; In EQL, this is the furthest we can go, but it doesn't imply that
               ;; the value returned is a scalar, it could be a map!
               :prop "SCALAR"
               ;; A join is always an object
               :join "OBJECT"
               (throw
                (ex-info
                 (format "Cannot infer GraphQL type kind (%s), EQL type not matched" (:type node))
                 {:type (:type node)})))
        description (get-in node [:meta :crux.graphql/description])]

    (cond->
        {:kind kind}

      true
      (conj [:name
             (case (:type node)
               :root "Root"
               :prop "String"
               :join
               (or
                (get-in node [:meta :crux.graphql/name])
                (graphql-name (:dispatch-key node))))])

      description
      (conj [:description description])

      (#{"OBJECT" "INTERFACE"} kind)
      (conj [:fields (mapv eql-ast-node-to-graphql-field (:children node))])

      (#{"OBJECT"} kind)
      ;; TODO
      (conj [:interfaces []])

      (#{"INTERFACE" "UNION"} kind)
      ;; TODO
      (conj [:possibleTypes []])

      (#{"ENUM"} kind)
      ;; TODO
      (conj [:enumValues []])

      (#{"INPUT_OBJECT"} kind)
      ;; TODO
      (conj [:inputFields []])

      (#{"NON_NULL" "LIST"} kind)
      ;; TODO
      (conj [:ofType {}])

      )))

(defn eql-ast-node-to-graphql-types [node]
  (vec
   (distinct
    (case (:type node)
      :root
      (concat
       [(eql-ast-node-to-graphql-type node)]
       (mapcat eql-ast-node-to-graphql-types (:children node))
       )
      :prop
      [(eql-ast-node-to-graphql-type node)]
      :join
      (concat
       [(eql-ast-node-to-graphql-type node)]
       (mapcat eql-ast-node-to-graphql-types (:children node))
       )
      (throw
       (ex-info
        "Cannot extract GraphQL type, EQL type not matched"
        {:type (:type node)}))))))


;; An idea for a directive
;; (reap/decode reap-graphql/Document "query @at(validTime:\"2019-10-10\") { greeting }")
