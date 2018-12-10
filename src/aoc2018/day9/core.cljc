(ns aoc2018.day9.core
  (:require [aoc2018.util :as u]
            [clojure.set :as set]
            [com.rpl.specter :refer :all]
            [net.cgrand.xforms :as xf]))


(def input {:players 446 :last-marble 71522})

(defn sens
  [cercle n]
  (mod (dec n) (count cercle)))

(defn contresens
  [cercle n]
  (sens cercle (- n)))

(defn focus
  [n cercle]
  (->> (split-at n cercle)
       reverse
       (apply concat)))

(defn ajouter-bille
  [cercle bille]
  (if (zero? (mod bille 23))
    {:score (+ bille (nth cercle (contresens cercle 7)))
     :cercle (->> cercle
                  (setval (nthpath (contresens cercle 7)) NONE)
                  (#(focus (contresens % 7) %)))}
    {:score 0
     :cercle (->> cercle
                  (setval (before-index (sens cercle 2)) bille)
                  (#(focus (sens cercle 2) %)))}))
