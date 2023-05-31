(ns euler.prime-by-num)

(defn prime-number
  [number]
  (letfn [(prime? [val] (not-any? zero? (map (fn [n] (rem val n)) (take-while #(<= % (Math/sqrt val)) (iterate inc 2)))))]
    (last (take number (filter prime? (iterate inc 2))))))