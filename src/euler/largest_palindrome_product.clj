(ns euler.largest-palindrome-product)

(defn palindrome?
  [val]
  (= (seq (str val)) (reverse (str val))))

(defn find-largest-palindrome-product
  [from to]
  (->> (for [x (range from (inc to)) y (range from (inc to))] (* x y))
       (filter palindrome?)
       (apply max)))