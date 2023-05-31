(ns euler.longest-collatz-sequence)

(defn find-longest-collatz-sequence
  [max-start]
  (letfn [(do-even [n] (/ n 2))
          (do-odd [n] (inc (* 3 n)))
          (collatz-sequence [start] (loop [current start
                                           result []]
                                      (cond
                                        (= 1 current) (conj result 1)
                                        (even? current) (recur (do-even current) (conj result current))
                                        :else (recur (do-odd current) (conj result current)))))]
    (first (sort-by :count > (map #(hash-map :id % :count (count (collatz-sequence %))) (range 1 max-start))))))