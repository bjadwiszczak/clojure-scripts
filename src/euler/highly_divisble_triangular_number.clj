(ns euler.highly-divisble-triangular-number)

(defn find-triangle-number-with-divisor
  [from-number min-divisors-count]
  (letfn [(all-divisors [num] (let [smaller (filter #(zero? (rem num %)) (range 1 (Math/sqrt num)))]
                                (concat smaller (map #(/ num %) smaller))))]
    (let [from-sum (/ (* from-number (inc from-number)) 2)]
      (loop [sum from-sum
             val from-number]
        (if (< min-divisors-count (count (all-divisors sum)))
          sum
          (recur (+ sum (inc val)) (inc val)))))))