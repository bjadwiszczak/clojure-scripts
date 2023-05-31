(ns clojure4.medium.balancing-brackets)

(defn my-balancing-brackets
  [sentence]
  (letfn [(match []
            (reduce #(if (and (= (first %1) ({\} \{ \) \( \] \[} %2))
                              ((comp not nil?) (first %1)))
                      (rest %1)
                      (conj %1 %2)
                      ) '() (filter #{\} \{ \) \( \] \[} (seq sentence))))]
    (empty? (match))))