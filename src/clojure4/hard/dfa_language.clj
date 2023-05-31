(ns clojure4.hard.dfa-language)

(defn my-dfa-language
  [dfa-def]
  (letfn [(get-step [coll [k v]]
            (into coll (create-next-steps ((dfa-def :transitions) k) v)))
          (create-next-steps [transitions state-vals]
            (mapv (fn [[letter state]] {state (mapv #(str % letter) state-vals)}) transitions))
          (generate-strings [states]
            (lazy-seq
              (let [next-steps (apply merge-with concat (reduce get-step [] states))
                    accepted (reduce into [] (vals (filter (fn [[k _]] ((dfa-def :accepts) k)) next-steps)))]
                (if (nil? next-steps)
                  accepted
                  (concat accepted (generate-strings next-steps))))))]
    (generate-strings {(dfa-def :start) '("")})))


;accepted (keys (filter (fn [[_ v]] ((dfa-def :accepts) v)) transitions))
;finalized-strings
;all-strings (for [s (vals steps) t (keys transitions)] (str s t))]