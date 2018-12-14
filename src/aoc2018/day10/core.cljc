(ns aoc2018.day10.core
  (:require [aoc2018.util :as u]
            [clojure.set :as set]
            [taoensso.tufte :refer [defnp p] :as tufte]
            [com.rpl.specter :refer :all]
            [net.cgrand.xforms :as xf]))

(def input "resources/day10/input1")
(def exemple "resources/day10/exemple1")

;; OK on a plein de points, ils bougent, à un moment ils font apparaitre un message qui disparait.
;; Il va nous falloir
;; - Parser l'entrée
;; - Coder l'avancement des points
;; - Coder un affichage écran
;; - Faire une heuristique pour trouver les moments intéressants ou un message
;;   pourrait apparaître.

;; Parser l'entrée
(defn parse-ligne
  [s]
  (->> s
       (re-find #"position=<(.*), (.*)> velocity=<(.*), (.*)>")
       (drop 1)
       (map clojure.string/trim)
       (map #(Integer/parseInt %))
       (apply #(assoc {} :position [%1 %2] :velocity [%3 %4]))))

(assert (= (parse-ligne "position=< 42211, -52498> velocity=<-4,  5>")
           {:position [42211 -52498] :velocity [-4 5]}))

;; Et le transducteur qui va faire ça
(def parse (u/parser parse-ligne))

;; L'avancement est facile à coder comme un map sur tous les points
(defn avancer
  [{[x y] :position [vx vy] :velocity :as point}]
  (assoc point :position [(+ x vx) (+ y vy)]))

(def pas-de-temps (map avancer))

;; l'affichage est en fait un peu pénible, parce qu'on ne connaît pas nécessairement
;; la taille d'écran nécessaire. On va extraire la taille d'écran
(def taille-ecran
  (comp (map :position)
        (xf/transjuxt {:min-x (comp (map first) xf/min)
                       :max-x (comp (map first) xf/max)
                       :min-y (comp (map second) xf/min)
                       :max-y (comp (map second) xf/max)})))

(eduction (comp parse taille-ecran) (u/lines exemple))
(eduction (comp parse taille-ecran) (u/lines input))

;; faire un ecran vierge de cette taille
(def ecran-vide
  (comp
   taille-ecran
   (xf/for [i %
           x (range (:min-x i) (inc (:max-x i)))]
    (vec (for [y (range (:min-y i) (inc (:max-y i)))]
           :empty)))
   (xf/into [])))

;; et on combine le tout pour afficher les points sur l'ecran
(def ecran
  (comp (xf/transjuxt [taille-ecran ecran-vide (xf/into [])])
        (mapcat (fn [[taille ecran points]]
                  (reduce (fn [screen {[x y] :position}]
                            (assoc-in screen [(- x (:min-x taille))
                                              (- y (:min-y taille))] :point))
                          ecran points)))))

(time (count (into [] (comp parse ecran) (u/lines exemple))))

;; la fonction qui permet d'afficher un ecran dans la console
(defn afficher-ecran
  [ecran]
  (doseq [ligne (apply map vector ecran)] ;; me suis planté dans les X et les Y
    (println
     (first
      (eduction (comp (map #(case % :empty "." :point "#"))
                      xf/str)
                ligne)))))

;; Bien sur le problème c'est que si on regarde la taille de l'écran
;; de l'entrée, il est très grand (de l'ordre de 100000x100000 aïe aïe aïe).
;; Après comme on suppose que les points vont se rapprocher pour former des lettres
;; on suppose aussi que l'écran va être beaucoup plus petit à ce moment là.

;; Maintenant il nous faut une heuristique pour savoir quand les points forment un mot.
;; Un bon indice serait effectivement d'observer l'évolution de la taille de l'écran en fonction
;; du temps, comme tous les points partent en ligne droite, la taille de l'écran devrait décroître
;; pendant la phase de convergence vers la phrase, puis croître après. Essayons.

(defn cmp-taille?
  [cmp taille1 taille2]
  (letfn [(surface [{:keys [:min-x :max-x :min-y :max-y]}]
            (* (- max-x min-x)
               (- max-y min-y)))]
    (cmp (surface taille1) (surface taille2))))

(defn heuristique-taille
  [points]
  (loop [taille (first (eduction taille-ecran points))
         points points]
    (let [nouveaux-points (map avancer points)
          nouvelle-taille (first (eduction taille-ecran nouveaux-points))]
      (if (cmp-taille? > nouvelle-taille taille)
        points
        (recur nouvelle-taille nouveaux-points)))))

;; OK, pas mal, mais c'est lent. Probablement parce que on calcule l'avancement pour
;; un nombre stupide de pas. On peut plutôt faire une recherche dichotomique en ajustant
;; le pas d'avancement :
(defn avancer-par
  [pas {[x y] :position [vx vy] :velocity :as point}]
  (assoc point :position [(+ x (* pas vx)) (+ y (* pas vy))]))

;; Il faut faire attention, parce qu'en faisant des gros pas, on peut manquer le bon
;; arrêt. Il faut se souvenir de deux arrêts en arrière pour ne pas se tromper.
(defn heuristique-taille2
  [pas points]
  (loop [taille (first (eduction taille-ecran points))
         points points
         avt-der-taille taille
         avt-der-points points
         pas pas]
    (let [nouveaux-points (map (partial avancer-par pas) points)
          nouvelle-taille (first (eduction taille-ecran nouveaux-points))]
      (if (cmp-taille? > nouvelle-taille taille)
        (if (<= pas 1)
          points
          (recur avt-der-taille avt-der-points avt-der-taille avt-der-points (/ pas 2)))
        (recur nouvelle-taille nouveaux-points taille points pas)))))

(defn solution1
  [input]
  (->> input
       (u/lines)
       (into [] parse)
       (heuristique-taille2 128)
       (into [] ecran)
       afficher-ecran))

;; Wow, c'était la bonne heuristique. J'en avais une autre en tête mais beaucoup plus chère,
;; qui consistait à vérifier que chaque point est dans le voisinage d'un autre point.

;; La seconde partie veut savoir combien de temps il faut attendre. Avec le chemin qu'on a pris,
;, rien de sorcier. Il suffit d'ajouter un compteur d'avancement dans notre boucle.
(defn heuristique-taille3
  [pas points]
  (loop [secondes-ecoulees 0 ;; c'est le seul ajout
         taille (first (eduction taille-ecran points))
         points points
         avt-der-secondes-ecoulees 0
         avt-der-taille taille
         avt-der-points points
         pas pas]
    (let [nouveaux-points (map (partial avancer-par pas) points)
          nouvelle-taille (first (eduction taille-ecran nouveaux-points))]
      (if (cmp-taille? > nouvelle-taille taille)
        (if (<= pas 1)
          secondes-ecoulees
          (recur avt-der-secondes-ecoulees ;; retour dans le passé
                 avt-der-taille avt-der-points
                 avt-der-secondes-ecoulees
                 avt-der-taille avt-der-points
                 (/ pas 2)))
        (recur (+ secondes-ecoulees pas)
               nouvelle-taille nouveaux-points
               secondes-ecoulees
               taille points
               pas)))))

(defn solution2
  [input]
  (->> input
       (u/lines)
       (into [] parse)
       (heuristique-taille3 128)))
