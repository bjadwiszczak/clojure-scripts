(ns leetcode.sum-3)

(defn sum-3
  [array]
  (let [indexes (partition 2 (interleave array (range)))]
    (into #{}
          (for [[a i] indexes
                [b j] indexes
                [c z] indexes
                :when (distinct? i j z)
                :when (= 0 (+ a b c))]
            (sort [a b c])))))