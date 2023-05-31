(ns clojure4.medium.palindromic-numbers)

(defn reverse-digits
  [num result]
  (if
    (zero? (quot num 10))
    (+ (mod num 10) (* result 10))
    (reverse-digits (quot num 10) (+ (mod num 10) (* result 10)))))

(defn get-next-top-down
  [[down _]]
  [(* 10 down) (dec (* 100 down))])

(defn create-palindromes
  [[down top]]
  (lazy-cat
    (lazy-cat
      (map #(reverse-digits % (quot % 10)) (range down (inc top)))
      (map #(reverse-digits % %) (range down (inc top))))
    (create-palindromes (get-next-top-down [down top]))))

(defn get-top-down
  [num]
  (let [down-total (reduce * (rest (repeat (/ (count (str num)) 2) 10)))
        down (if (even? (count (str num)))
               down-total
               (quot num down-total))
        top (dec (* 10 down-total))]
    [down top]))

(defn my-palindromic-numbers
  [num]
  (filter #(>= % num)
          (if (zero? num)
            (lazy-cat
              '(0)
              (create-palindromes [1 9]))
            (create-palindromes (get-top-down num)))))