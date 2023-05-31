(ns euler.power-digit-sum)

(defn power-of-two-digit-sum
  [power-num]
  (apply +' (map #(- (int %) 48) (seq (str (apply *' (repeat power-num 2)))))))
