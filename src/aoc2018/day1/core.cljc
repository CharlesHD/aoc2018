(ns aoc2018.day1.core
  (:require [aoc2018.util :as u]
            [net.cgrand.xforms :as xf]))

;; L'entrée est une liste de nombres entiers positifs ou négatifs.
;; Le problème consiste à trouver la somme de ces nombres en partant de 0.
(def input "resources/day1/input1")
(def parse (u/parser read-string))

(defn solution1
  [input]
  (transduce parse + 0 (u/lines input)))

(println (solution1 input))

;; La deuxième partie du problème consiste à trouver la première somme partielle redondante.
;; Si aucun nombre n'est répété après une première passe, il faut considérer que l'entrée se répète
;; autant de fois que nécessaire jusqu'à tomber retomber sur une somme partielle déjà calculée.

;; Il faut donc garder en mémoire les sommes partielles calculées.

;; d'abord on commence par coder une fonction qui détecte un doublon
(defn reduit-doublon
  [vues nouvelle]
  (if (vues nouvelle) ;; si l'objet appartient à l'ensemble des valeurs déjà vues
    (reduced nouvelle) ;; c'est le doublon qu'on cherche
    (conj vues nouvelle))) ;; sinon c'est une nouvelle valeur à ajouter !

(def premier-doublon
  (xf/reduce (completing reduit-doublon) #{}))

(defn solution2
  [input]
  (first
   (eduction (comp parse
                   (xf/reductions + 0) ;; reductions permet de considérer les sommes partielles
                   premier-doublon) ;; on s'arrête au premier doublon trouvé
             (cycle (u/lines input))))) ;; cycle permet de répéter au début de séquence à volonté

(println (solution2 input))
