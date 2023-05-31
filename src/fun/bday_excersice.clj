(ns fun.bday-excersice)

(defn bday-fun
  [input-words]
  (map #(partition 2 1 [(int (first %))] (map int %)) input-words))
