(ns euler.sum-square-difference)

(defn find-sum-square-difference
  [to-num]
  (letfn [(sum-of-squares [coll] (reduce + (map #(* % %) coll)))
          (square-of-sum [coll] (let [sum (reduce + coll)]
                                  (* sum sum)))]
    (let [coll (range 1 (inc to-num))
          sum-of-squares (sum-of-squares coll)
          square-of-sum (square-of-sum coll)]
      [square-of-sum sum-of-squares (- square-of-sum sum-of-squares)])))
