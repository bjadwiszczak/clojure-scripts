(ns clojure4.hard.latin-square-slicing)


(defn my-latin-square-slicing
  [original-table]
  (let [longest-width (first (sort > (map count original-table)))
        width longest-width
        height (count original-table)
        all-starts (for [x (range 0 width)
                         y (range 0 height)]
                     [x y])
        last-shift (map #(- longest-width (count %)) original-table)
        first-shift (repeat (count last-shift) 0)]
    (letfn [(get-by [table [x y]]
              (get (get table y nil) x nil))
            (get-right [[x y]]
              [(inc x) y])
            (get-down [[x y]]
              [x (inc y)])
            (get-coll [table start size move-fun]
              (loop [result []
                     current start
                     counter 0]
                (if (= counter size)
                  result
                  (recur (conj result (get-by table current)) (move-fun current) (inc counter)))))
            (get-row [table start size]
              (get-coll table start size get-right))
            (get-column [table start size]
              (get-coll table start size get-down))
            (get-all-rows [table start size]
              (map #(get-row table % size) (take size (iterate get-down start))))
            (get-all-rows-as-sets [table start size]
              (map #(into #{} %) (get-all-rows table start size)))
            (get-all-columns [table start size]
              (map #(get-column table % size) (take size (iterate get-right start))))
            (get-all-columns-as-sets [table start size]
              (map #(into #{} %) (get-all-columns table start size)))
            (latin-square? [table start size]
              (let [latin-set (into #{} (get-row table start size))]
                (if (or (not (= (count latin-set) size)) (contains? latin-set nil))
                  false
                  (empty? (filter #(not (= latin-set %)) (concat (get-all-rows-as-sets table start size) (get-all-columns-as-sets table start size)))))))
            (get-possible-check-sizes [[x y]]
              (let [max-width (- width x)
                    max-height (- height y)
                    max-possible (min max-width max-height)]
                (range max-possible 1 -1)))
            (get-sizes-of-latin-squares [table start]
              (filter #(latin-square? table start %) (get-possible-check-sizes start)))
            (get-latin-square [table start]
              (map #(get-all-rows table start %) (get-sizes-of-latin-squares table start)))
            (get-amounts-by-size [latin-suares]
              (frequencies (map #(count (first %)) latin-suares)))
            (get-next-shift [last-shift current-shift]
              (let [reversed-last (reverse last-shift)
                    reversed-current (reverse current-shift)]
                (loop [current-el-last (first reversed-last)
                       rest-last (rest reversed-last)
                       current-el-current (first reversed-current)
                       rest-current (rest reversed-current)
                       result []]
                  (if (> current-el-last current-el-current)
                    (reverse (into (conj result (inc current-el-current)) rest-current))
                    (if (empty? rest-last)
                      last-shift
                      (recur (first rest-last)
                             (rest rest-last)
                             (first rest-current)
                             (rest rest-current)
                             (conj result 0)))))))
            (shift-table [table shifts]
              (mapv #(into (into [] (repeat %1 nil)) %2) shifts table))]
      (loop [current-shift first-shift
             result #{}]                                    ;
        (if (= current-shift last-shift)
          (get-amounts-by-size (into result (mapcat #(get-latin-square (shift-table original-table current-shift) %) all-starts)))
          (recur (get-next-shift last-shift current-shift)
                 (into result (mapcat #(get-latin-square (shift-table original-table current-shift) %) all-starts))))))))