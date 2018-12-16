(ns aoc2018.day15.core
  (:require [aoc2018.util :as u]
            [clojure.set :as set]
            [taoensso.tufte :refer [defnp p] :as tufte]
            [com.rpl.specter :refer :all]
            [net.cgrand.xforms :as xf]))

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
  (for [y (range (count lignes))
        x (range (count (first lignes)))
        :let [unite (get-in lignes [y x])]
        :when (not= unite :vide)]
    (case unite
      :mur {:type :mur :position [x y]}
      :gobelin {:type :gobelin :pv 200 :atk 3 :position [x y]}
      :elfe {:type :elfe :pv 200 :atk 3 :position [x y]})))
