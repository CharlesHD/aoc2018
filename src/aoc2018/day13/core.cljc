(ns aoc2018.day13.core
  (:require [aoc2018.util :as u]
            [clojure.set :as set]
            [taoensso.tufte :refer [defnp p] :as tufte]
            [com.rpl.specter :refer :all]
            [net.cgrand.xforms :as xf]))

(def input "resources/day13/input1")
(def exemple "resources/day13/exemple1")

(defn parse-char
  [c]
  (case c
    \/ {:type :diagonal :direction :bas-haut}
    \\ {:type :diagonal :direction :haut-bas}
    \| {:type :vertical}
    \- {:type :horizontal}
    \+ {:type :intersection}
    \> {:type :charriot :direction :droite}
    \< {:type :charriot :direction :gauche}
    \v {:type :charriot :direction :bas}
    \^ {:type :charriot :direction :haut}
    {:type :empty}))

(defn print-elem
  [{t :type d :direction}]
  (case t
    :diagonal (case d
                :bas-haut \/
                :haut-bas \\)
    :vertical \|
    :horizontal \-
    :intersection \+
    :charriot (case d
                :droite \>
                :gauche \<
                :haut \^
                :bas \v)
    :empty \space))

(defn parse-ligne
  [s]
  (mapv parse-char s))

(def parse (u/parser parse-ligne))

(defn inspecter
  [carte [x y]]
  (get-in carte [y x] {:type :empty}))

(defn dimensions
  [carte]
  (let [hauteur (count carte)
        largeur (apply max (map count carte))]
    [largeur hauteur]))

;; Il faut localiser et stocker la position et la mémoire des charriots.
(defn charriots
  [carte]
  (let [[l h] (dimensions carte)]
    (for [x (range l)
          y (range h)
          :let [elem (inspecter carte [x y])]
          :when (= (:type elem) :charriot)]
      {:position [x y] :dernier-virage :droite :direction (:direction elem)})))

(defn etat
  [carte]
  (let [chars (charriots carte)
        carte-sans-chars
        (transform [ALL ALL #(= (:type %) :charriot)]
                   (fn [{d :direction}]
                     (case d
                       (:droite :gauche) {:type :horizontal}
                       (:haut :bas) {:type :vertical}))
                   carte)]
    {:charriots chars
     :carte carte-sans-chars}))

(defn prochain-virage
  [dernier-virage]
  (case dernier-virage
    :droite :gauche
    :gauche :devant
    :devant :droite))

(defn direction-diagonale
  [dir diag-dir]
  (case [dir diag-dir]
    [:gauche :haut-bas] :haut
    [:droite :haut-bas] :bas
    [:bas :haut-bas] :droite
    [:haut :haut-bas] :gauche
    [:droite :bas-haut] :haut
    [:gauche :bas-haut] :bas
    [:haut :bas-haut] :droite
    [:bas :bas-haut] :gauche
    (throw (ex-info "wtf dir" {:dir dir :diag-dir diag-dir}))))

(defn avancer
  [{:keys [carte charriots] :as state}
   {[x y] :position dv :dernier-virage dir :direction :as char}]
  (let [autres-charriots (map :position charriots)
        prochaine-case (case dir
                         :droite [(inc x) y] :gauche [(dec x) y]
                         :haut [x (dec y)] :bas [x (inc y)])
        prochain-elem (inspecter carte prochaine-case)
        collision? (some #{prochaine-case} autres-charriots)]
    (->
     (case (:type prochain-elem)
       :diagonal  (assoc char :direction
                         (direction-diagonale dir (:direction prochain-elem)))
       :intersection (let [nv (prochain-virage dv)]
                       (assoc char
                              :dernier-virage nv
                              :direction
                              (case nv
                                :devant dir
                                :droite (case dir
                                          :haut :droite
                                          :bas :gauche
                                          :droite :bas
                                          :gauche :haut)
                                :gauche (case dir
                                          :haut :gauche
                                          :bas :droite
                                          :droite :haut
                                          :gauche :bas))))
       char)
     (assoc :position prochaine-case)
     (update :collisions
             (fnil (fn [v pc] (if collision? (conj v pc) v)) [])
             prochaine-case))))

(defn pas-de-temps
  [etat]
  (reduce
   (fn [etat char]
     (let [nchar (avancer etat char)]
       (if (seq (:collisions nchar))
         (reduced nchar)
         (setval [:charriots ALL #(= (:position %) (:position char))] nchar etat))))
   etat
   (sort-by (comp vec reverse :position)
            (:charriots etat))))

(defn solution
  [input]
  (let [etat (etat (into [] parse (u/lines input)))]
    (some :collisions (iterate pas-de-temps etat))))

;; Maintenant à chaque crash, les deux charriots sont enlevés. On continue jusqu'à ce qu'il n'en reste
;; qu'un. On veut sa position.

;; On modifie un peu pas-de-temps
(defn pas-de-temps2
  [etat]
  ;; s'il n'y a plus qu'un charriot avant le prochain pas de temps
  (if (>= 1 (count (:charriots etat)))
    {:resultat (:charriots etat)} ;; on retourne ça
    ;; sinon on refait presque tout comme avant
    (reduce
     (fn [etat char]
       ;; d'abord il faut bien vérifier que le chariot est toujours là
       (if-not (some #{char} (:charriots etat))
         etat ;; s'il n'est plus là, il ne se passe rien
         (let [nchar (avancer etat char)]
           (if (seq (:collisions nchar))
             ;; on supprime les charriots qui se sont rentrés dedans
             (setval [:charriots ALL #( #{(:position char)
                                          (:position nchar)}
                                       (:position %))] NONE etat)
             (setval [:charriots ALL #(= (:position %) (:position char))] nchar etat)))))
     etat
     (sort-by (comp vec reverse :position)
              (:charriots etat)))))

(def exemple2 "resources/day13/exemple2")
(defn solution2
  [input]
  (let [etat (etat (into [] parse (u/lines input)))]
    (some :resultat (iterate pas-de-temps2 etat))))
