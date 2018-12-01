(ns aoc2018.day1.core
  (:require [clojure.java.io :as io]))

(def input "resources/day1/input1")
(count (line-seq (io/reader input)))

(defn solution1
  [input]
  (transduce (map read-string) + 0 (line-seq (io/reader input))))

(solution1 input)
