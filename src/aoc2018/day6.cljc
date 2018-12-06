(ns aoc2018.day6.core
  (:require [aoc2018.util :as u]
            [net.cgrand.xforms :as xf]))

(def input "resources/day6/input1")

(def parse (u/parser (fn [l] (read-string (str "[" l "]"))))) ;; oui c'est un read-string hack


;; Bonjour problème de géométrie
;; On a donc un ensemble de points sur la grille des couple d'entiers
;; Il s'agit de trouver le point de l'entrée qui a le plus grand territoire
;; d'influence fini. Un point de la grille est dans le territoire d'influence
;; d'un point de l'entrée s'il s'agit du SEUL point le plus proche. Les points
;; équidistants ne sont dans aucun territoire d'influence.

;; la distance est la distance de Manhattan, c'est à dire le nombre de déplacements
;; d'un pas (horizontal ou vertical) qu'il faut pour aller du premier point au second.
;; Elle s'appelle distance de Manhattan (ou du taxi) parce que c'est la distance que mets
;; une voiture pour aller d'un croisement de rue à un autre dans une ville à plan carré comme c'est
;; le cas à New York et Manhattan. #Trivia

(defn distance
  [[x y] [x' y']] ;; c'est la somme du
  (+ (Math/abs (- x x')) ;; nombre de déplacements horizontaux
     (Math/abs (- y y')))) ;; et du nombre de déplacements verticaux

;; Mon intuition est que le problème est beaucoup plus simple une fois qu'on a évincé
;; les points qui ont une influence infinie. Je dirais qu'il s'agit des points de l'enveloppe
;; convexe.
;; L'enveloppe convexe est l'ensemble de points qui forme un polygone tel que tous les autres
;; points sont dedans. C'est un problème bien connu de géométrie algorithmique.
;; On peut implémenter Quickhull par exemple qui nécessite les notions de distance à une droite
;; et d'être en-dessous et au-dessus d'une droite.

;; on va définir plein d'opération de géométrie. Ça a l'air compliqué mais c'est juste du vocabulaire.
;; Pour définir si on est au-dessus ou au-dessous d'un point on utilise une relation d'ordre
;; x * y' < x' * y
(defn linearite
  [[x y] [x' y']]
  (- (* x y') (* x' y)))

(def dessous? (comp neg? linearite))
(def dessus? (comp pos? linearite))
(def colineaire? (comp zero? linearite))

(defn minus
  [[x y] [x' y']]
  [(- x x') (- y y')])

(defn dessous-droite?
  [[d1 d2] p]
  (dessous? (minus p d1) (minus d2 d1)))

(defn dessus-droite?
  [[d1 d2] p]
  (dessus? (minus p d1) (minus d2 d1)))

(defn sur-droite?
  [[d1 d2] p]
  (colineaire? (minus p d1) (minus d2 d1)) )

;; on a aussi besoin de bien définir la droite
(defn droite
  [p p']
  [p p'])

;; et la distance à la droite
(defn- equation
  [[[x y] [x' y']]]
  [(- y' y) (- x x') (- (* x' y)
                        (* x y'))])
(defn- distance-droite
  [d [x y]]
  (let [[a b c] (equation d)]
    (Math/abs (+ (* a x) (* b y) c))))

(declare quickhull)
(defn- quickhull-dessus
  [d pts]
  (quickhull d (filter (partial dessus-droite? d) pts)))
(defn- quickhull-dessous
  [d pts]
  (quickhull d (filter (partial dessous-droite? d) pts)))

(defn quickhull
  ([[p1 p2 :as d] pts]
   (if (empty? pts)
     []
     (let [pmax (apply max-key (partial distance-droite d) pts)
           d1 (droite p1 pmax)
           d2 (droite pmax p2)
           qh (if (dessus-droite? d pmax)
                quickhull-dessus
                quickhull-dessous)]
         (concat (qh d1 pts)
                 [pmax]
                 (qh d2 pts)))))
  ([pts]
   (let [pmin (apply min-key first pts)
         pmax (apply max-key first pts)
         d (droite pmin pmax)] ;; On commence par trouver les points d'abscisses min et max
     (println d)
     (concat [pmin]
             (quickhull-dessus d pts)
             [pmax]
             (reverse (quickhull-dessous d pts))))))


;; le point qu'on cherche est celui qui est le plus éloigné de tous les autres :
;; il a le plus de place pour s'étendre au sein de l'enveloppe.
(defn cumul-distance
  [pts p]
  (transduce (map (partial distance p)) + pts))

(defn n-voisinage
  [[x y :as p] n]
  (concat
   (for [f [+ -]
         i [0 1]]
     (update p i f n))
   (for [i (range 1 n)
         x-sign [+ -]
         y-sign [+ -]]
     [(x-sign x i) (y-sign y (- n i))])))

(defn exclusif
  [n vois autres]
  (remove (fn [p] (some #(<= (distance p %) n) autres)) vois))

(defn influence
  [p autres]
  (loop [c 0
         n 1]
    (let [vois (n-voisinage p n)
          add (count (exclusif n vois autres))]
      (println c)
      (if (zero? add)
        c
        (recur (+ c add) (inc n))))))

(defn add-points-on-hull
  [hull pts]
  (let [droites (conj (partition 2 1 hull) ((juxt last first) hull))
        on-hull (fn [p] (some (fn [d] (sur-droite? d p)) droites))]
    (distinct
     (concat hull
             (filter on-hull pts)))))

(defn solution1
  [input]
  (let [pts (->> input u/lines (sequence parse))
        env (quickhull pts)
        env (set (add-points-on-hull env pts))
        inside (remove (set env) pts)
        inf (apply max-key #(cumul-distance pts %) inside)]
    (influence inf (remove #{inf} pts))))

;; brr c'est terriblement lent. Surement parce qu'il existe des considérations
;; beaucoup plus pragmatique vis à vis de la distance manhattan.

;; la deuxième partie considère maintenant qu'on veut la zone des points qui sont
;; proches de tous les points de l'entrée. Où être proche signifie être à une distance
;; cumulée à tous les points inférieure à 10000.

(defn critere
  [pts p]
  (< (cumul-distance pts p) 10000))

(defn solution2
  [input]
  (let [pts (->> input u/lines (sequence parse))
        terrs (territoires pts)
        xf-sol (comp (filter (partial critere pts))
                     xf/count)]
    (eduction xf-sol terrs)))

;; toujours turbolent. Construire et traiter tout terrs c'est stupide

(defn- coord-barycentrique
  [p triangle]
  (let [[[a1 a2] [b1 b2] [c1 c2]] triangle
        [p1 p2] p
        bc2 (- b2 c2)
        ac1 (- a1 c1) ac2 (- a2 c2)
        cb1 (- c1 b1)
        pc1 (- p1 c1)
        pc2 (- p2 c2)
        det (+ (* bc2 ac1)
               (* cb1 ac2))
        l1 (/ (+ (* bc2 pc1) (* cb1 pc2)) det)
        l2 (/ (+ (* (- ac2) pc1) (* ac1 pc2)) det)
        l3 (- 1 l1 l2)]
    [l1 l2 l3]))

(defn triangle-contient
  [triangle p]
  (every? pos? (coord-barycentrique p triangle)))
