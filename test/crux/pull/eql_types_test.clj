;; Copyright © 2020, JUXT LTD.

(ns crux.pull.eql-types-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [crux.pull.alpha.graphql :as graphql]
   [edn-query-language.core :as eql]))

(deftest eql-ast-node-to-graphql-types-test
  (testing "Root"
    (let [types (graphql/eql-ast-node-to-graphql-types
                 (eql/query->ast []))]
      (is (= 1 (count types)))
      (is (= {:kind "OBJECT"
              :name "Root"
              :fields []
              :interfaces []}
             (first types))))

    (testing "setting the Root object type's description"
      (let [types (graphql/eql-ast-node-to-graphql-types
                   (eql/query->ast
                    (with-meta
                      []
                      {:graphql/type {:description "This is the root"}})))]
        (is (= 1 (count types)))
        (is (= {:kind "OBJECT"
                :name "Root"
                :description "This is the root"
                :fields []
                :interfaces []}
               (first types))))))

  (testing "Properties"
    (let [types (graphql/eql-ast-node-to-graphql-types
                 (eql/query->ast
                  [:album/name :album/year]))]
      (is (=
           (+ 1 ;; root
              1 ;; scalar string
              )
           (count types)))

      ;; Expecting a Root OBJECT type, followed by two SCALAR types.
      (is (= ["OBJECT" "SCALAR"] (map #(get % :kind) types)))
      (is (= "Root" (get-in types [0 :name])))
      (is (= "String" (get-in types [1 :name]))))

    (testing "override the GraphQL field's name with metadata"
      (let [types
            (graphql/eql-ast-node-to-graphql-types
             (eql/query->ast
              [:album/name
               (with-meta
                 '(:album/year)
                 {:graphql/field {:name "released"}})]))]

        (is (= "released" (get-in types [0 :fields 1 :name])))))

    (testing "provide the GraphQL field's description with metadata"
      (let [types (graphql/eql-ast-node-to-graphql-types
                   (eql/query->ast
                    [:album/name
                     (with-meta
                       '(:album/year)
                       {:graphql/field
                        {:name "released"
                         :description "The year the album was released"}})]))]

        (is (= "The year the album was released"
               (get-in types [0 :fields 1 :description]) )))))

  (testing "Joins"
    (let [types
          (graphql/eql-ast-node-to-graphql-types
           (eql/query->ast
            [{:favorite-albums
              [:album/name :album/year]}]))]

      ;; Expecting a root OBJECT type, one for favorite-albums, and the SCALAR String
      (is
       (=
        ["OBJECT" "OBJECT" "SCALAR"]
        (map #(get % :kind) types)))))

  (testing "Args"
    (is
     (=
      {:name "artist",
       :description
       "Filter albums that have an `album__artist` fields which matches this argument value, if given.",
       :type {:kind "SCALAR" :name "String"}}
      (get-in
       (let [schema
             (eql/query->ast
              [^{:lookup (fn [_ _])
                 :params {"artist"
                          {:graphql/description "Filter albums that have an `album__artist` fields which matches this argument value, if given."
                           :graphql/type
                           {:kind "SCALAR"
                            :name "String"}}}}
               {:favoriteAlbums
                [(with-meta '(:album/name)
                   {:lookup (fn [_ _])})
                 (with-meta '(:album/artist)
                   {:lookup (fn [_ _])})
                 (with-meta '(:album/year)
                   {:lookup (fn [_ _])})]}

               ])]
         (graphql/eql-ast-node-to-graphql-types schema))
       [0 :fields 0 :args 0])))))
