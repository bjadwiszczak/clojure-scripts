(ns euler.factorial-digit-sum)

(defn factorial
  [value]
  (->> (range 1 (inc value))
       (reduce *')))

(defn count-factorial-digit-sum
  [value]
  (->> (factorial value)
       (str)
       (seq)
       (map #(Character/digit % 10))
       (reduce +')))