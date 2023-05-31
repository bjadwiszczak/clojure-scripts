(ns euler.summation-of-primes)

(defn find-sum-of-primes-below
  [bound]
  (letfn [(prime? [val] (not-any? zero? (map (fn [n] (rem val n)) (take-while #(<= % (Math/sqrt val)) (iterate inc 2)))))]
    (reduce + (take-while #(< % bound) (filter prime? (iterate inc 2))))))