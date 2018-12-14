(ns aoc2018.day9.core
  (:require [aoc2018.util :as u]
            [clojure.set :as set]
            [taoensso.tufte :refer [defnp p] :as tufte]
            [com.rpl.specter :refer :all]
            [net.cgrand.xforms :as xf]))

(def input {:players 446 :last-marble 71522})

(defnp sens
  [taille-cercle n]
  (mod n taille-cercle))

(defnp contresens
  [taille-cercle n]
  (sens taille-cercle (- n)))

(defnp avant-apres
  [v taille n]
  [(subvec v 0 n) (subvec v n taille)])

(defnp ajouter-bille
  [{:keys [cercle taille]} bille]
    (if (zero? (mod bille 23))
      (p :mod23
         (let [[avant [enleve & apres]] (avant-apres cercle taille (contresens taille 7))]
           {:score (+ bille enleve)
            :taille (dec taille)
            :cercle (vec (concat apres avant))}))
      (p :notmod23
         {:score 0
          :taille (inc taille)
          :cercle (let [[avant apres] (split-at (sens taille 2) cercle)]
                    (vec (concat [bille] apres avant )))})))

(assert (= (:cercle (ajouter-bille {:cercle [0] :taille 1} 1))
           [1 0]))
(assert (= (:cercle (reduce ajouter-bille {:cercle [0] :taille 1} [1 2]))
           [2 1 0]))
(assert (= (:cercle (reduce ajouter-bille {:cercle [0] :taille 1} [1 2 3]))
           [3 0 2 1]))
(assert (= (:cercle (reduce ajouter-bille {:cercle [0] :taille 1} [1 2 3 4]))
           [4 2 1 3 0]))

(defn partie
  [nb-joueurs bille-max]
  (reduce (fn [etat bille]
            (let [{:keys [score taille cercle]} (ajouter-bille etat (inc bille))
                  joueur (mod bille nb-joueurs)]
              (-> etat
                  (assoc :cercle cercle :taille taille)
                  (update-in [:scores joueur] (fnil + 0) score))))
          {:cercle [0]
           :taille 1
           :scores {}}
          (range bille-max)))

(defn solution1
  [nb-joueurs bille-max]
  (->> (partie nb-joueurs bille-max)
       :scores
       vals
       (apply max)))

(defn insert-index
  [[prev-idx taille i]]
  (if (zero? (mod i 23))
    [(mod (- prev-idx 7) taille) (dec taille) (inc i)]
    [(mod (+ prev-idx 2) taille) (inc taille) (inc i)]))

(def cercle-et-score
  (xf/reduce
   (completing
    (fn [[v score] [i _ c]]
      (if (and (pos? (dec c)) (zero? (mod (dec c) 23)))
        [(setval (nthpath i) NONE v)
         (conj score (nth v i nil))]
        [(setval (before-index i) (dec c) v)
         score])))
   [[] []]))

(defn xfsol
  [nb-billes nb-joueurs]
  (comp
   (take nb-billes)
   cercle-et-score
   (mapcat second)
   (map-indexed (fn [i x] (+ x 23 (* i 23))))
   (map-indexed vector)
   (xf/by-key (fn [[i _]] (mod i nb-joueurs))
              second (xf/reduce +))))

(defn solution1
  [nb-billes nb-joueurs]
  (into {} (xfsol nb-billes nb-joueurs) (iterate insert-index [0 1 1])))
