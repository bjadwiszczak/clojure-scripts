(ns euler.smallest-multiple)

(defn prime?
  [val]
  (->> (range 2 (inc (/ val 2)))
       (map #(rem val %))
       (not-any? zero?)))

(defn power-of-prime-smaller-than
  [to-number prime]
  (->> (iterate (partial * prime) prime)
       (take-while #(< % to-number))
       (last)))

(defn smallest-multiple
  [to-number]
  (->> (iterate inc 2)
       (filter prime?)
       (take-while #(< % to-number))
       (map #(power-of-prime-smaller-than to-number %))
       (reduce *)))



; old ugly solution
(defn find-smallest-multiple
  [to-num]
  (letfn [(prime? [val] (not-any? zero? (map (fn [n] (rem val n)) (take-while #(<= % (Math/sqrt val)) (iterate inc 2)))))
          (primes [] (filter prime? (iterate inc 2)))
          (find-divide-pair [[_ num]]
            (if (= 1 num)
              [1 1]
              (let [divider (first (filter #(zero? (rem num %)) (primes)))]
                [divider (/ num divider)])))
          (find-all-dividers [num]
            (frequencies
              (map first
                   (->> (iterate find-divide-pair [0 num])
                        (take-while #(not (= [1 1] %)))
                        (rest)))))]
    (reduce (fn [res [key val]] (* res (reduce * (repeat val key))))
            1 (apply merge-with max (map find-all-dividers (range 1 (inc to-num)))))))