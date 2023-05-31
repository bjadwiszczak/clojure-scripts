(ns euler.largest-prime-factor)

(defn prime?
  [val]
  (->> (range 2 (inc (/ val 2)))
       (map #(rem val %))
       (not-any? zero?)))

(defn find-largest-prime-factor
  [num]
  (->> (iterate (fn [current] (dec current)) (int (Math/sqrt num)))
       (filter #(zero? (rem num %)))
       (filter prime?)
       (take-while #(> % 1))
       (first)))