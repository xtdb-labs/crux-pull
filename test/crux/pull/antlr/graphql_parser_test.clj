;; Copyright © 2020, JUXT LTD.

(ns crux.pull.antlr.graphql-parser-test
  (:require
   [clj-antlr.core :as antlr]
   [juxt.reap.alpha.graphql :as reap-graphql]
   [juxt.reap.alpha.api :as reap]
   [clojure.java.io :as io]
   [clojure.walk :refer [postwalk]]))

(comment
  (require '[criterium.core :as crit]))

(comment
  (def parser
    (antlr/parser
     (slurp (io/resource "grammars/GraphQL.g4"))
     {:format :sexp})))

(comment
  (crit/quick-bench
   (postwalk
    (fn [x]
      (if (seq? x)
        (case (first x)
          :intValue (Integer/parseInt (second x))
          :arguments (let [v (into [] x)] [:arguments (subvec v 2 (dec (count v)))])
          :argument (let [v (into [] x)] [(get-in v [1 1]) (get-in v [3])])
          :selectionSet [:selection-set (into [] (remove string? (rest x)))]
          ;;       :field [:field (into {} (rest x))]
          :value (second x)
          (into [] x)
          )
        x))
    (parser "query { foo(a: 1, c: 2) { bar zip quz}}"))))

;; clj-antlr with into [] postwalk
;; Evaluation count : 10056 in 6 samples of 1676 calls.
;;              Execution time mean : 62.542921 µs
;;     Execution time std-deviation : 1.712715 µs
;;    Execution time lower quantile : 60.999914 µs ( 2.5%)
;;    Execution time upper quantile : 65.261909 µs (97.5%)
;;                    Overhead used : 6.947411 ns

(comment
  (crit/quick-bench
   (postwalk
    (fn [x]
      (if (seq? x)
        (case (first x)
          :intValue (Integer/parseInt (second x))
          :arguments (let [v (into [] x)] [:arguments (subvec v 2 (dec (count v)))])
          :argument (let [v (into [] x)] [(get-in v [1 1]) (get-in v [3])])
          :selectionSet [:selection-set (into [] (remove string? (rest x)))]
          ;;       :field [:field (into {} (rest x))]
          :value (second x)
          x
          )
        x))
    (parser "query { foo(a: 1, c: 2) { bar zip quz}}"))))

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers

;; clj-antlr WITHOUT into [] postwalk
;; Evaluation count : 10614 in 6 samples of 1769 calls.
;;              Execution time mean : 57.182785 µs
;;     Execution time std-deviation : 1.132951 µs
;;    Execution time lower quantile : 55.891830 µs ( 2.5%)
;;    Execution time upper quantile : 58.595437 µs (97.5%)
;;                    Overhead used : 6.947411 ns

;; No post-processing
(comment
  (crit/quick-bench
   (parser "query { foo(a: 1, c: 2) { bar zip quz}}")))

;; Evaluation count : 18930 in 6 samples of 3155 calls.
;;              Execution time mean : 33.439882 µs
;;     Execution time std-deviation : 1.281864 µs
;;    Execution time lower quantile : 32.520936 µs ( 2.5%)
;;    Execution time upper quantile : 35.413788 µs (97.5%)
;;                    Overhead used : 6.947411 ns




(comment
  (crit/quick-bench
   (reap/decode reap-graphql/Document "query { foo(a: 1, c: 2) { bar zip quz}}")))

;; Evaluation count : 15030 in 6 samples of 2505 calls.
;;              Execution time mean : 40.408262 µs
;;     Execution time std-deviation : 981.369683 ns
;;    Execution time lower quantile : 38.914284 µs ( 2.5%)
;;    Execution time upper quantile : 41.429564 µs (97.5%)
;;                    Overhead used : 6.947411 ns
