;; Copyright © 2020, JUXT LTD.

(ns crux.pull.eql-query-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [crux.pull.alpha.eql :refer [exec validate prepare-query]]
   [edn-query-language.core :as eql])
  (:import (crux.pull.alpha.eql Resolver)))

(deftest root-property-test
  (is
   (=
    [[:greeting "Hello World!"]]
    (exec
     (reify Resolver
       (lookup [_ ctx ast opts]
         (case [ctx (:key ast)]
           [nil :greeting] "Hello World!")))
     nil
     (eql/query->ast '[:greeting])
     {})))

  (testing "non-existent property"
    (is
     (=
      [[:greeting "Hello World!"]
       [:dummy ::not-found]]
      (exec
       (reify Resolver
         (lookup [_ ctx ast opts]
           (get {:greeting "Hello World!"} (:key ast) ::not-found)))
       nil
       (eql/query->ast '[:greeting :dummy])
       {})))))

;; Low-level: Call EQL directly, no validation, properties not found yield nil results.
;; Used by 'road builders' - schema construction
(deftest eql-exec-test
  (is
   (=
    [[:greeting "Hello"] [:audience "World"]]
    (exec
     (reify Resolver
       (lookup [_ ctx ast opts]
         (get {:greeting "Hello"
               :audience "World"} (:key ast))))
     nil
     (eql/query->ast '[:greeting :dummy :audience])
     {}))))

;; Mid-level: Query with EQL, but the EQL is subject to validation according to the schema
;; Used by Clojure app developers, e.g. UI devs
(deftest eql-validation-test
  (is
   (=
    [{:error {:message "Property not in schema", :property :dummy}}]
    (validate
     (eql/query->ast '[:greeting :audience])
     (eql/query->ast '[:greeting :dummy :audience])
     {})))

  (is
   (nil?
    (validate
     (eql/query->ast '[:greeting :audience])
     (eql/query->ast '[:greeting :audience])
     {})))

  ;; Join schema - work-in-progress
  ;; Work on validation of joins
  #_(validate
     (eql/query->ast
      '[{:favorite-albums
         [:album/name :album/year]}])

     ;; Bad query
     (eql/query->ast
      '[{:albums
         [:album/name]}])
     {}))

(deftest prepare-query-test
  (let [results
        [{:album/name "In Rainbows"
          :album/artist "Radiohead"
          :album/year 2007}
         {:album/name "OK Computer"
          :album/artist "Radiohead"
          :album/year 1997}
         {:album/name "Kraftwerk"
          :album/artist "Autobahn"
          :album/year 1974}]
        schema
        (eql/query->ast
         [(with-meta
            {:favoriteAlbums
             [(with-meta '(:album/name)
                {:lookup (fn [ctx] (get ctx :album/name))})
              (with-meta '(:album/artist)
                {:lookup (fn [ctx] (get ctx :album/artist))})
              (with-meta '(:album/year)
                {:lookup (fn [ctx] (get ctx :album/year))})
              ]}
            {:lookup (fn []
                       ;; For example, do some datalog query.
                       ;; For this test, just return results
                       results)})])
        query (eql/query->ast
               [{:favoriteAlbums [:album/name :album/year]}])
        result (prepare-query query schema)]

    (is result)
    (is (get-in result [:children 0 :meta :lookup]))
    (is (get-in result [:children 0 :children 0 :meta :lookup]))
    (is (get-in result [:children 0 :children 1 :meta :lookup]))))
