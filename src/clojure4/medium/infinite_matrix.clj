(ns clojure4.medium.infinite-matrix)

(defn my-infinite-matrix
  ([fun x y max-x max-y]
   (take max-x (map #(take max-y %) (my-infinite-matrix fun x y))))
  ([fun]
   (my-infinite-matrix fun 0 0))
  ([fun x y]
   (letfn [(my-range [x] (lazy-seq (cons x (my-range (inc x)))))
           (create-row [fun x y]
             (lazy-seq
               (cons (fun x y) (create-row fun x (inc y)))))
           (create-columns [fun x y]
             (map #(create-row fun % y) (my-range x)))]
     (create-columns fun x y))))