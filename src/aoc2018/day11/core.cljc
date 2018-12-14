(ns aoc2018.day11.core
  (:require [aoc2018.util :as u]
            [clojure.set :as set]
            [taoensso.tufte :refer [defnp p] :as tufte]
            [com.rpl.specter :refer :all]
            [net.cgrand.xforms :as xf]))

(def input 6042)

;; Cette fois il y a un calcul tordu pour calculer le niveau d'énergie d'une
;; cellule sur un écran 300x300.

(defn niveau-energie
  [x y serie]
  (-> (+ x 10)
      (* y)
      (+ serie)
      (* (+ x 10))
      (quot 100)
      (mod 10)
      (- 5)))

(assert (= (niveau-energie 3 5 8) 4))
(assert (= (niveau-energie 122 79 57) -5))
(assert (= (niveau-energie 217 196 39) 0))
(assert (= (niveau-energie 101 153 71) 4))
;; OK. Ça marche.

;; Maintenant le problème est de trouver la zone de 3x3 case avec le niveau d'énergie cumulé
;; le plus élevé, indiquée par le coin supérieur gauche. Les index commencent à 1, et finissent à 300
;; pour x et y.

;; Bon on y va comme un bourrin pour commencer.
(defn niveau-cumulee-zone
  [x y serial]
  (reduce +
          (for [i (range 0 3)
                  j (range 0 3)]
            (niveau-energie (+ x i) (+ y j) serial))))

(defn solution1
  [input]
  (apply max-key last
         (for [i (range 1 299) j (range 1 299)]
           [i j (niveau-cumulee-zone i j input)])))

(assert (= (solution1 18) [33 45 29]))
(assert (= (solution1 42) [21 61 30]))

;; OK c'était pas bien compliqué. Let's étape 2.

;; ALLRIGHT, maintenant on intègre de la combinatoire. On peut avoir des carrés de n'importe
;; quelle taille entre 1 et 300 maintenant. C'est à dire une dimension en plus
(defn niveau-cumulee-zone2
  [x y taille serial]
  (reduce +
          (for [i (range 0 taille)
                j (range 0 taille)]
            (niveau-energie (+ x i) (+ y j) serial))))

;; On peut encore essayer de bourriner, mais je suppose qu'on est voué à l'eternité du calcul.
(defn solution2
  [input]
  (apply max-key last
         (for [i (range 1 299) j (range 1 299) s (range 1 (- 301 (max i j)))]
           [i j s (niveau-cumulee-zone2 i j s input)])))

;; (time (solution2 18))
;; erk c'est long. Ça tient pas en une minute bouuuh.

;; Déjà on peut constater qu'on recalcule plein de zones déjà connues. Pour ce genre de
;; situation on peut utiliser le super pouvoir de la mémoïsation.
;; hop on coupe le calcul d'une région en 4
;; +-----+-----+
;; |     |     |
;; |     |     |
;; +-----+-----+
;; |     |     |
;; |     |     |
;; +-----+-----+
;; et on compte chaque région, quand la taille est impaire, il y a
;; le carré du milieu qui chevauche une fois.
(def niveau-cumulee-zone-mem
  (memoize
   (fn [x y taille serial]
     (if (<= taille 1)
       (niveau-energie x y serial)
       (let [left (quot taille 2)
             right (- taille left)
             offset (if (odd? taille) (- (niveau-energie (+ x left) (+ y left) serial)) 0)]
         (+ (niveau-cumulee-zone-mem x y left serial) ;; carre haut gauche
            (niveau-cumulee-zone-mem x (+ y left) right serial) ;; bas gauche
            (niveau-cumulee-zone-mem (+ x left) y right serial) ;; haut droite
            (niveau-cumulee-zone-mem (+ x right) (+ y right) left serial) ;; bas droite
            offset))))))

;; On reessaye de bourriner ?
(defn solution3
  [input]
  (apply max-key last
         (for [i (range 1 299) j (range 1 299) s (range 1 (- 301 (max i j)))]
           [i j s (niveau-cumulee-zone-mem i j s input)])))

;; (time (solution3 18))
;; ARGH toujours pas. Enfin on le fait finir en ~60secondes.
;; L'espace de couverture est trop grand (300^3 pfiou)

;; Est-ce que ce serait pas l'heure de diviser et de conquérir ?
;; Par exemple, le meilleur carré est soit dans la moitié gauche de la grille, soit dans
;; la moitié droite, soit à cheval sur la ligne médiane ?
;; Quelle est la complexité de ce calcul ?
;; Si on note T(largeur-max, )

(defn solve-middle
  [input xmin piv xmax]
  (apply max-key last
         (for [i (range xmin piv)
               j (range 0 299)
               s (range (- piv i) (- xmax i))]
           [i j s (niveau-cumulee-zone-mem i j s input)])))

(defn solve
  [input xmin xmax]
  (if (>= xmin xmax)
    (apply max-key last (for [j (range 1 299)]
                          [xmin j 1 (niveau-energie xmin j input)]))
    (let [pivot (quot (- xmax xmin) 2)]
      (max (solve input xmin pivot)
           (solve input (inc pivot) xmax)
           (solve-middle input xmin (inc pivot) xmax)))))

(assert (= (solution2 18) [90 269 16 113]))
