(ns clojure4.easy.last-element)

(defn my-last-element
  [coll]
  (first (reverse coll)))