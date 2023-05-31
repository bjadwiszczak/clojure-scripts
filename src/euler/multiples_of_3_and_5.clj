(ns euler.multiples-of-3-and-5)

(defn multiples
  [ns max-val]
  (letfn [(multi-of [n max-val] (take-while #(< % max-val) (iterate (partial + n) n)))]
    (apply + (into #{} (mapcat #(multi-of % max-val) ns)))))