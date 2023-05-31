(ns euler.even-fibo-numbers)

(defn even-fibo
  []
  (->> (iterate (fn [[a b]] [b (+ a b)]) [1 2])
       (map first)
       (filter even?)
       (take-while #(< % 4000000))
       (reduce +)))