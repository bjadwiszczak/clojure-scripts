(ns euler.number-letter-counts)

(defn get-digit [value index]
  (Character/digit (nth (seq (reverse (str value))) (dec index)) 10))

; max 1000
(defn count-letters
  [from to]
  (let [digits {1 "one" 2 "two" 3 "three" 4 "four" 5 "five" 6 "six" 7 "seven" 8 "eight" 9 "nine"}
        teens {10 "ten" 11 "eleven" 12 "twelve" 13 "thirteen" 14 "fourteen" 15 "fifteen"
               16 "sixteen" 17 "seventeen" 18 "eighteen" 19 "nineteen"}
        tens {20 "twenty" 30 "thirty" 40 "forty" 50 "fifty" 60 "sixty" 70 "seventy" 80 "eighty" 90 "ninety"}
        bigs {3 "hundred" 4 "thousand"}]
    (letfn [(get-numeral [value]
              (let [numeral (first (filter (complement nil?) (vector (digits value) (teens value) (tens value))))]
                (if (nil? numeral)
                  ""
                  numeral)))
            (known-numeral? [value] (not (empty? (get-numeral value))))
            (get-big-numeral [value size]
              (when (not (zero? value))
                (str (digits value) "" (bigs size))))
            (create-tens-numeral [value]
              (if (known-numeral? value)
                (get-numeral value)
                (let [ten (* 10 (int (/ value 10)))
                      digit (- value (* 10 (int (/ value 10))))]
                  (str (tens ten) "" (digits digit)))))
            (create-big-numeral [value]
              (let [thousand (if (= 4 (count (str value)))
                               (get-big-numeral (get-digit value 4) 4)
                               "")
                    hundred (get-big-numeral (get-digit value 3) 3)
                    tens (create-tens-numeral (- value (* 100 (int (/ value 100)))))]
                (str thousand hundred
                     (if (not (= tens ""))
                       (str "and" tens)
                       ""))))
            (create-numeral [value]
              (let [size (count (str value))]
                (cond
                  (= 1 size) (digits value)
                  (= 2 size) (create-tens-numeral value)
                  :default (create-big-numeral value))))]
      (reduce + (map #(count (create-numeral %)) (range from (inc to)))))))