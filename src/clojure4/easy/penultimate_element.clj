(ns clojure4.easy.penultimate-element)

(defn my-penultimate-element
  [coll]
  (second (reverse coll)))