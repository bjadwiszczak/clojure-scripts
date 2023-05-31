(ns clojure4.hard.tree-reparenting)

(defn my-tree-reparenting
  [new-parent tree]
  (let [all-nodes (tree-seq seq? rest tree)
        starting-node (first (filter #(= new-parent (first %)) all-nodes))]
    (letfn [(collect-nodes [result current-node]
              (if (= (first tree) (first (first result)))
                result
                (let [next-node (first (filter #((set %) current-node) all-nodes))
                      reparented (remove #(= current-node %) next-node)]
                  (collect-nodes (cons reparented result) next-node))))
            (create-new-tree [node-list]
              (reduce #(seq (conj (vec %2) %1)) (first node-list) (rest node-list)))]
      (create-new-tree (collect-nodes (cons starting-node '()) starting-node)))))