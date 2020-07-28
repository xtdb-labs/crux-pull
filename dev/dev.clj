;; Copyright © 2020, JUXT LTD.

(ns dev
  (:require
   [crux.api :as crux]
   [dev-extras :refer [system go reset]]))

(defn crux []
  (:juxt.crux.ig/system system))

(defn q
  ([query] (q query nil))
  ([query vt]
   (crux/q
    (crux/db (crux) vt)
    query)))

(defn e [eid & [vt]]
  (crux/entity
   (crux/db (crux) vt)
   eid))

(defn submit-tx [ops]
  (crux/submit-tx
   (crux)
   ops))
