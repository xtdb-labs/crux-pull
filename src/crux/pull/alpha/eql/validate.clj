;; Copyright © 2020, JUXT LTD.

(ns crux.pull.alpha.eql.validate)

(defmulti validate (fn [schema ctx ast opts] (:type ast)))

(defmethod validate :root [schema ctx ast opts]
  (seq (vec (keep #(validate (set (:children schema)) ctx % opts) (:children ast)))))

(defmethod validate :prop [schema ctx ast opts]
  (when-not (contains? schema ast)
    {:error {:message "Property not in schema"
             :property (:key ast)}}))
