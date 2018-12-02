(ns aoc2018.day2.core
  (:require [aoc2018.util :as u]
            [net.cgrand.xforms :as xf]))

(def input "resources/day2/input1")

(def xf-freq (xf/by-key identity xf/count))
;; Le problème consiste à compter la redondance des lettres sur chaque ligne.
;; On commence par créer un parser qui transforme les lignes en fréquences
(def parse (u/parser frequencies))

;; Ce qui est important pour chaque ligne, c'est de savoir s'il y a un motif qui apparaît
;; exactement n fois, et ce indépendemment du motif.
(defn frequence-des-motifs
  [freq]
  (->> freq vals distinct))

;; Ensuite on veut produire un checksum. Le procédé est simplement la multiplication
;; de la fréquence des fréquences de motifs des lignes.
;; Si notre entrée a 10 lignes avec des fréquences de motifs 3 et 20 avec des
;; motifs de fréquences 2, le checksum vaut 10x20.
;; Les fréquences de motifs valant 1 sont ignorées dans cette multiplication !

;; D'abord on transforme nos motifs de frequences en leur frequence
(def xf-freq-motifs
  (comp (mapcat frequence-des-motifs) ;; on les concatène tous
        (remove #{1}) ;; on vire les 1
        xf-freq
        (map second))) ;; on compte les frequences du tout

(defn checksum
  [input]
  (transduce (comp parse
                 xf-freq-motifs)
              * (u/lines input)))

(checksum input)


;; La seconde partie est totalement différente. Il s'agit de trouver deux lignes
;; qui ne diffèrent que d'un seul caractère, à la même position.
;; Il s'agit donc de trouver les deux lignes dont la distance d'édition est de 1.
;; La solution la plus simple est de parcourir toutes les paires de ligne et trouver
;; la première de distance 1
(defn distance
  [s1 s2]
  (->> (map not= s1 s2) ;; on considère les différences et similitudes
       (filter identity) ;; on vire les similitudes
       (count))) ;; on compte
(assert (= (distance "abc" "xyz") 3))
(assert (= (distance "abc" "ayc") 1))
(assert (= (distance "abc" "abc") 0))

(defn similitude
  [s1 s2]
  (apply str
         (sequence (comp (map #(if (= %1 %2) %1 nil))
                         (remove #{nil}))
                   s1 s2)))
(assert (= (similitude "abc" "xyz") ""))
(assert (= (similitude "abc" "ayc") "ac"))
(assert (= (similitude "abc" "abc") "abc"))

;; La solution naïve
(defn naive
  [input]
  (let [lines (u/lines input)]
    (->> (for [i lines j lines :when (not= i j)] ;; on parcourt les paires de lignes
           [i j (distance i j)])
         (sequence (xf/minimum #(compare (last %1) (last %2)))) ;; on trouve celle de distance min
         first
         (take 2)
         (apply similitude)))) ;; on en extrait la similitude, c'est notre résultat.

;; On peut certainement faire plus rapide que O(n^2)
(time (naive input))

;;
