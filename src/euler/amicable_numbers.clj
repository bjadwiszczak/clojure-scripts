(ns euler.amicable-numbers)


(defn sum-of-all-dividers
  [value]
  (->> (range 1 (inc (/ value 2)))
       (filter #(zero? (mod value %)))
       (reduce +)))

(defn amicable?
  [dividers-map value]
  (let [dividers-sum (dividers-map value)
        pair-dividers-sum (dividers-map dividers-sum)]
    (and (not= value dividers-sum) (= value pair-dividers-sum))))

(defn create-dividers-map
  [to]
  (->> (range 1 (inc to))
       (map #(vector % (sum-of-all-dividers %)))
       (into {})))

(defn sum-of-amicable-numbers
  [to]
  (let [dividers-map (create-dividers-map to)]
    (->> (range 1 (inc to))
         (filter #(amicable? dividers-map %))
         (reduce +))))