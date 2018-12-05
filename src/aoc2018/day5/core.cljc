(ns aoc2018.day5.core
  (:require [aoc2018.util :as u]
            [net.cgrand.xforms :as xf]))

(def input "resources/day5/input1")

;; Cette fois l'entrée n'est pas une suite de ligne, mais une longue sequence
;; de caractères. Il faut dissocier si ce caractère est en minuscule ou majuscule.

(defn parse-char
  "Traduit un caractère en unité du polymer avec sa polarité"
  [c]
  (let [lc (first (clojure.string/lower-case c))]
    {:unit lc
     :polarite (if (= c lc)
                 :lower :upper)}))

(def parse (u/parser parse-char))

;; Ensuite il y a des règles de destructions d'unités au sein du polymer
;; les unités opposées qui se suivent se détruisent
(defn opposed?
  [u1 u2]
  (and (= (:unit u1) (:unit u2))
       (not= (:polarite u1) (:polarite u2))))

;; on va traiter sequentiellement les unités
(defn ajouter-unite-droite
  [rpolymer unite] ;; pour des soucis de vitesse, on renverse le polymer pour profiter de la liste
  (let [dernier (first rpolymer)]
    (cond
      (nil? dernier) (list unite)
      (opposed? dernier unite) (rest rpolymer)
      :sinon (conj rpolymer unite))))

(def polymer-reaction
  (comp (xf/reduce (completing ajouter-unite-droite) [])
        (map reverse)))

(def xf-sol
  (comp polymer-reaction
        (mapcat identity)
        xf/count))

(defn solution1
  [input]
  (->> input
       (u/lines)
       first
       (into [] (comp parse xf-sol))
       first))

(time (solution1 input))

;; la partie deux demande le même résultat mais en ayant le droit de supprimer
;; un type d'unite de tout le polymer avant la reaction. Il faut trouver la chaine
;; la plus courte après réaction avec cette opération permise en plus.

;; d'abord il faut lister tous les types d'unités
(defn unites
  [polymer]
  (into #{} (map :unit) polymer))

;; et créer le filtre
(defn unit-filter
  [c]
  (remove #(= (:unit %) c)))

(defn solution2
  [input]
  (let [polymer (into [] parse (-> input u/lines first))
        xf (xf/transjuxt (map (fn [c] (comp (unit-filter c) xf-sol))
                              (unites polymer)))]
    (apply min (first (into [] xf polymer)))))

(time (solution2 input))
