(ns aoc2018.day12.core
  (:require [aoc2018.util :as u]
            [clojure.set :as set]
            [taoensso.tufte :refer [defnp p] :as tufte]
            [com.rpl.specter :refer :all]
            [net.cgrand.xforms :as xf]))

(def initial-state
  "#........#.#.#...###..###..###.#..#....###.###.#.#...####..##..##.#####..##...#.#.....#...###.#.####")

(def rules
  "#..## => .
   ##..# => #
   ..##. => .
   .##.# => #
   ..... => .
   ..### => #
   ###.# => #
   #.... => .
   #.##. => #
   .#.## => #
   #...# => .
   ...## => .
   ###.. => #
   .#..# => .
   ####. => .
   ....# => .
   ##### => #
   .###. => .
   #..#. => .
   ##... => #
   .#... => #
   #.#.# => .
   ..#.. => #
   ...#. => #
   ##.#. => .
   .##.. => #
   .#.#. => .
   #.#.. => .
   ..#.# => #
   #.### => .
   ##.## => .
   .#### => #")

(def exemple-init "#..#.#..##......###...###")
(def exemple-rules
  "...## => #
  ..#.. => #
  .#... => #
  .#.#. => #
  .#.## => #
  .##.. => #
  .#### => #
  #.#.# => #
  #.### => #
  ##.#. => #
  ##.## => #
  ###.. => #
  ###.# => #
  ####. => #")

;; OK c'est des règles pour transformer des . en # et inversement selon leur voisinage.
;; Donc c'est un automate cellulaire, sur un bandeau infini.
(defn parse-rule
  [rule]
  (->> rule
       (re-find #"(.)(.)(.)(.)(.) => (.)")
       (drop 1)
       ((fn [[lm l c r rm res]]
          {:gauche [l lm]
           :droite [r rm]
           :etat c
           :resultat res}))))

;; La fonction voisinage d'un point, donne le voisinage necessaire pour nos règles
(defn voisinage
  [bandeau idx]
  (letfn [(etat [i] (nth bandeau i "."))]
    {:gauche [(etat (- idx 1)) (etat (- idx 2))]
     :droite [(etat (+ idx 1)) (etat (+ idx 2))]
   :etat (etat idx)}))

(defn generation
  [regles]
  (reduce (fn [m regle]
            (assoc m (select-keys regle [:gauche :droite :etat])
                   (:resultat regle)))
          {} regles))

(defn creer-bandeau
  "On ne peut pas stocker un bandeau infini, du coup on l'étend à droite et à gauche d'une certaine
   taille."
  [initial taille]
  (concat (repeat taille ".")
          (map str initial)
          (repeat taille ".")))

;; Allez on a tout ce qu'il faut
(defn avancer-generations
  [initial rules-str nb-generations]
  (let [bandeau (creer-bandeau initial (+ nb-generations 2))
        rgls (->> rules-str clojure.string/split-lines (map parse-rule) generation)
        regles (fn [x]
                 (get rgls x "."))
        mise-a-jour (comp regles voisinage)]
    (loop [band bandeau
           gen 0]
      (println (somme band nb-generations))
      (if (= gen nb-generations)
        band
        (recur
         (map-indexed
          (fn [idx etat]
            (if (<= 2 idx (- (count band) 3)) ;; on ne s'intéresse pas aux deux premiers
              (mise-a-jour band idx)            ;; et deux derniers
              etat))
          band)
         (inc gen))))))


(defn somme
  [resultat gen]
  (let [sol (comp (drop 2)
                 (xf/drop-last 2)
                 (map-indexed vector)
                 (remove (comp #{"."} second))
                 (map first)
                 (map #(- % gen)))]
    (transduce sol + resultat)))

(defn solution
  [initial rules-str nb-gen]
  (let [resultat (avancer-generations initial rules-str nb-gen)]
    (somme resultat nb-gen)))

;; OK, facile de faire la somme jusqu'à la 20ème génération.

;; Maintenant on nous demande de compter la somme jusqu'à la 5e10 ème génération.
;; Si on regarde comment cette fonction somme avance, on remarque qu'elle se stabilise aux
;; alentours de la 90ème génération. Les règles de l'exemple se stabilisent aussi dans le même
;; nombre de génération (mais sur un pas différent).
;; Je n'écris pas l'algo pour trouver ce motif, mais dans l'idée, on peut itérer
;; l'avancement de génération, calculer la somme, et s'arrêter dès qu'on trouve deux ou
;; trois fois la même différence dans la somme de suite.
(def somme-100 8866)
(defn somme-n>100
  [n]
  (+ somme-100
     (* 80 (- n 100))))
