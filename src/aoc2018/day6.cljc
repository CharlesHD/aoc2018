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
(defn dessous?
  [[x y] [x' y']]
  (< (* x y') (* x' y)))
(defn dessus?
  [[x y] [x' y']]
  (> (* x y') (* x' y)))

(defn minus
  [[x y] [x' y']]
  [(- x x') (- y y')])

(defn dessous-droite?
  [[d1 d2] p]
  (dessous? (minus p d1) (minus d2 d1)))

(defn dessus-droite?
  [[d1 d2] p]
  (dessus? (minus p d1) (minus d2 d1)))

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


;; ok maintenant attribuons les points de la région [xmin xmax] X [ymin ymax]
;; à leur territoire d'influence respectif.
(defn influenceur
  [influenceurs p]
  (->> (group-by #(distance p %) influenceurs) ;; on groupe les influenceurs / leur distance au point
       (apply min-key key) ;; on prend les moins éloignés
       val
       (#(if (> (count %) 1) ;; s'il y a plus d'un influenceur à équidistance
           nil  ;; ce point est neutre
           (first %))))) ;; sinon l'influenceur gagne.

(defn territoires
  [pts]
  (let [[[xmin xmax]
         [ymin ymax]] (xf/transjuxt [(comp (map first) (xf/transjuxt [xf/min xf/max]))
                                     (comp (map second) (xf/transjuxt [xf/min xf/max]))] pts)]
    (for [i (range xmin (inc xmax))
          j (range ymin (inc ymax))]
      [i j])))

(defn solution1
  [input]
  (let [pts (->> input u/lines (sequence parse))
        terrs (territoires pts)
        env (quickhull pts)
        xf-sol (comp (map (partial influenceur pts))
                     (remove (set env))
                     (remove nil?)
                     (xf/by-key identity xf/count)
                     (map val))]
    (transduce xf-sol (completing max) 0 terrs)))

;; brr c'est terriblement lent. Surement parce qu'il existe des considérations
;; beaucoup plus pragmatique vis à vis de la distance manhattan.

;; la deuxième partie considère maintenant qu'on veut la zone des points qui sont
;; proches de tous les points de l'entrée. Où être proche signifie être à une distance
;; cumulée à tous les points inférieure à 10000.

(defn cumul-distance
 [pts p]
  (transduce (map (partial distance p)) + pts))

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
