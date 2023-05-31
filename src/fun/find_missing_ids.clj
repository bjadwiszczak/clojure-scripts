(ns fun.find-missing-ids
  (:require [clojure.xml :as xml])
  (:require [clojure.set :as sets])
  (:import (java.io ByteArrayInputStream)))


(defn compare-ids
  [path-before path-after]
  (with-open [ids-before-file (clojure.java.io/reader path-before)
              ids-after-file (clojure.java.io/reader path-after)]
    (let [ids-before (line-seq ids-before-file)
          ids-after (line-seq ids-after-file)]
      ;(str (count (into #{} ids-before)) " " (count ids-after))
      (sets/difference (into #{} ids-before) (into #{} ids-after))
      ;(->> (frequencies ids-before)
      ;     (filter (fn [[_ v]] (= v 2)))
      ;     (map first))
      )))
