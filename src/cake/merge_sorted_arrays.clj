(ns cake.merge-sorted-arrays)

(defn merge-sorted-arrays
  [array-a array-b]
  (loop [[a & remaining-a :as all-a] array-a
         [b & remaining-b :as all-b] array-b
         result []]
    (cond
      (and (empty? all-a) (empty? all-b))
        result
      (or (nil? b) (and (not (nil? a)) (< a b)))
        (recur remaining-a all-b (conj result a))
      :else
        (recur all-a remaining-b (conj result b)))))