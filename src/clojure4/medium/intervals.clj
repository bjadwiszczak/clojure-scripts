(ns clojure4.medium.intervals)

(defn my-intervals
  [coll]
  (letfn [(get-intervals [[f & the-rest]]
            (reduce (fn [[interval :as intervals] next-val] ()
                      (if (<= (dec next-val) (last interval))
                        (update-in intervals [0] conj next-val)
                        (into [[next-val]] intervals))
                      ) [[f]] the-rest))]
    (if ((comp not empty?) coll)
      (reverse (map (juxt first last) (get-intervals (sort < coll))))
      [])))