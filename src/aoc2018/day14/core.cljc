(ns aoc2018.day14.core
  (:require [aoc2018.util :as u]
            [clojure.set :as set]
            [taoensso.tufte :refer [defnp p] :as tufte]
            [com.rpl.specter :refer :all]
            [net.cgrand.xforms :as xf]))

(def input 894501)

(def start [3 7])

(defn step
  [[[i j] xs]]
  (let [v1 (nth xs i)
        v2 (nth xs j)
        score (+ v1 v2)]
    (let [nxs (if (> score 9)
                (conj xs 1 (mod score 10))
                (conj xs score))
          c (count nxs)
          maj #(mod (+ %1 (inc %2)) c)]
      [[(maj i v1) (maj j v2)] nxs])))

(defn solution
  [input nb]
  (->> (nth (iterate step [[0 1] start]) (+ input nb))
       second
       (split-at input)
       second
       (take 10)))

(defn compute
  [dist x]
  (nth (iterate step x) dist))

(defn solution2
  [input dist]
  (let [input (map (comp read-string str) (str input)) ;; oui c'est un hack
        taille-input (count input)
        xf-sol (comp
                (xf/partition taille-input 1)
                (take-while #(not= % input)))
        solve (fn [x] (count (sequence xf-sol (second x))))]
    (loop [x (compute dist [[0 1] start])
           c 0]
      (println c)
      (let [sol (solve x)]
        (if (= sol (count (partition taille-input 1 (second x))))
          (recur (compute dist x) (inc c))
          sol)))))
