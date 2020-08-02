;; Copyright © 2020, JUXT LTD.

(ns crux.pull.graphql-introspection
  (:require
   [edn-query-language.core :as eql]))

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
