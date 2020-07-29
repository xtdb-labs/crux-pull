;; Copyright © 2020, JUXT LTD.

(ns crux.pull.alpha.eql.validate)

(defmulti validate (fn [schema ast opts] (:type ast)))

(defmethod validate :root [schema ast opts]
  (seq (vec (keep #(validate (set (:children schema)) % opts) (:children ast)))))

(defmethod validate :prop [schema ast opts]
  (when-not (contains? schema ast)
    {:error {:message "Property not in schema"
             :property (:key ast)}}))
