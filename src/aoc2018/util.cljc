(ns aoc2018.util
  (:require [clojure.java.io :as io]
            [net.cgrand.xforms :as xf]))

(defn lines
  [input]
  (line-seq (io/reader input)))

(defn parser
  ([line-parse]
   (map line-parse))
  ([line-parse f]
   (comp (map line-parse)
         (xf/reduce f)))
  ([line-parse f init]
   (comp (map line-parse)
         (xf/reduce f init))))
