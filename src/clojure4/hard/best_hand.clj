(ns clojure4.hard.best-hand)

(defn my-best-hand
  [hand]
  (letfn [(into-ranks [] (sort (map (fn [[_ v]] ({\A 12 \K 11 \Q 10 \J 9 \T 8} v (- (int v) 50))) hand)))
          (color? [] (apply = (map first hand)))
          (straight? [] (let [ranks (into-ranks)]
                          (or (and (= 5 (count (into #{} ranks)))
                                   (= 4 (- (last ranks) (first ranks))))
                              (= ranks [0 1 2 3 12]))))
          (of-kind [val num] (= num (count ((group-by identity (vals (frequencies (map second hand)))) val))))]
    (cond
      (and (color?) (straight?)) :straight-flush
      (of-kind 4 1) :four-of-a-kind
      (and (of-kind 3 1) (of-kind 2 1)) :full-house
      (color?) :flush
      (straight?) :straight
      (of-kind 3 1) :three-of-a-kind
      (of-kind 2 2) :two-pair
      (of-kind 2 1) :pair
      :default :high-card)))