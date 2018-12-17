(ns aoc2018.day15.core
  (:require [aoc2018.util :as u]
            [clojure.set :as set]
            [taoensso.tufte :refer [defnp p] :as tufte]
            [com.rpl.specter :refer :all]
            [net.cgrand.xforms :as xf]))

(def exemple "resources/day15/exemple1")
(def input "resources/day15/input1")
(defn parse-char
  [c]
  (case c
    \. :vide
    \G :gobelin
    \E :elfe
    \# :mur))

(def parse (u/parser (partial mapv parse-char)))

(defn unites
  [lignes]
  (group-by
   :type
   (for [y (range (count lignes))
         x (range (count (first lignes)))
         :let [unite (get-in lignes [y x])]
         :when (not= unite :vide)]
     (case unite
       :mur {:type :mur :position [x y]}
       :gobelin {:type :gobelin :pv 200 :atk 3 :position [x y]}
       :elfe {:type :elfe :pv 200 :atk 3 :position [x y]}))))

(defn voisinage
  [[x y]]
  [[(inc x) y] [(dec x) y]
   [x (inc y)] [x (dec y)]])

(defn chemin
  ([cases-interdites case-1 case-2]
   (chemin cases-interdites case-1 case-2 []))
  ([cases-interdites case-1 case-2 chem]
   (if (= case-1 case-2)
     (conj chem case-1)
     (let [voisins (sort (remove cases-interdites (voisinage case-1)))]
       (first
        (sequence
         (comp (map #(chemin (conj cases-interdites case-1)
                             % case-2 (conj chem case-1)))
               (filter seq))
         voisins))))))
