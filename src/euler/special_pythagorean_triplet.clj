(ns euler.special-pythagorean-triplet)

(defn find-triplet
  [sum]
  (let [to-sum-range (range 0 sum)]
    (for [a to-sum-range
          b to-sum-range
          c to-sum-range
          :when (and (= 1000 (+ a b c))
                     (= (+ (* a a) (* b b)) (* c c)))]
      (* a b c))))