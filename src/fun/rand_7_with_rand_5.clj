(ns fun.rand-7-with-rand-5)

(defn rand-int-7 []
  (quot (->> #(rand-int 5)
             (repeatedly 2)
             (apply #(+ %1 (* 5 %2)))
             #()
             repeatedly
             (drop-while #(> % 20))
             first)
        3))
