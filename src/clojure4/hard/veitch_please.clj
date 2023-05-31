(ns clojure4.hard.veitch-please)

(defn my-veitch-please
  [func]
  (letfn [(compare-elem [bool-el to-simplify]
            (let [diff (clojure.set/difference bool-el to-simplify)
                  diff-other (clojure.set/difference to-simplify bool-el)]
              (when (and (= 1 (count diff))
                         (= (clojure.string/lower-case (first diff))
                            (clojure.string/lower-case (first diff-other))))
                (disj bool-el (first diff)))))
          (simplify-elem [bool-el bool-func]
            (keep #(compare-elem bool-el %) bool-func))
          (simplify-all [[bool-func result]]
            (let [after-simplification (map #(vector % (simplify-elem % bool-func)) bool-func)
                  simplified (get-simplified after-simplification)
                  not-simplified (concat result (get-not-simplified after-simplification))]
              [simplified not-simplified]))
          (get-not-simplified [simplified]
            (into #{} (keep #(when (empty? (second %))
                              (first %)) simplified)))
          (get-simplified [simplified]
            (into #{} (reduce concat #{} (keep #(when (not (empty? (second %)))
                                                 (second %)) simplified))))
          (from-subsets [result]
            (map #(filter (fn [current-res] (clojure.set/subset? current-res %)) result) func))]
    (loop [after-simplification [func #{}]]
      (if (empty? (first after-simplification))
        (into #{} (mapcat identity (filter #(= 1 (count %)) (from-subsets (second after-simplification)))))
        (recur
          (simplify-all after-simplification))))))
