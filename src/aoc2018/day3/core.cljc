(ns aoc2018.day3.core
  (:require [aoc2018.util :as u]
            [net.cgrand.xforms :as xf]))

(def input "resources/day3/input1")

;; Chaque ligne correspond à une commande de territoire
;; Elle est de la forme suivante :
;; #id @ x,y: wxh
;; avec id l'identifiant de la commande, x et y les coordonnées du point
;; haut gauche du rectangle et w et h les dimensions du rectangle.

(defn parse-fn
  [s]
  (->> s (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)") ;; on utilise une regex pour extraire les champs
       (drop 1) ;; détail vis à vis du retour de re-find
       (map read-string) ;; on transforme les strings en nombres
       (apply #(hash-map :id %1 :corner {:x %2 :y %3} :width %4 :height %5)))) ;; le tout dans une map.

(def parse (u/parser parse-fn))

;; la première question est de savoir combien de carrés de surface 1 sont concernés
;; par au moins deux commandes.

;; Une méthode naïve consiste à considérer tous les carrés demandés et
;; compter le nombre de fois qu'ils sont demandés.

(defn carres-concernes
  "Liste les carrés concernés par une commande"
  [{:keys [corner width height] :as commande}]
  (let [{:keys [x y]} corner]
    (for [i (range x (+ x width))
          j (range y (+ y height))]
      {:x i :y j})))

;; Maintenant il suffit de compter les carrés qui apparaissent deux fois
(def xf-sol1
  (comp parse
        (mapcat carres-concernes)
        (xf/by-key identity xf/count)
        (filter #(> (second %) 1))
        xf/count))

(defn solution1
  [input]
  (first (sequence xf-sol1 (u/lines input))))

(time (solution1 input))
;; C'est un peu long. On peut surement faire plus rapide sans énumérer les carrés.


;; La deuxième partie nous demande de trouver l'identifiant de la seule commande
;; qui ne chevauche aucune autre.
;; Ça doit pouvoir se faire en une seule réduction, mais avec un nombre de cas à vérifier
;; qui augmente linéairement.
;; D'abord on définit l'intersection :
(defn one-dim-intersect?
  "Intersection d'intervalles"
  [x w x' w']
  (let [y (dec (+ x w))
        y' (dec (+ x' w'))
        order (sort [x y x' y'])]
    (and (or (= y x')
             (not= order [x y x' y']))
         (or (= y' x)
             (not= order [x' y' x y]))))) ;; ou inclusion de [x w] dans [x' (x' + w')].
(assert (one-dim-intersect? 0 2 1 2)) ;; XX vs .XX
(assert (not (one-dim-intersect? 0 2 2 2))) ;; XX vs ..XX
(assert (not (one-dim-intersect? 2 2 0 2))) ;; ..XX vs XX
(assert (one-dim-intersect? 0 2 0 2)) ;; XX vs XX
(assert (one-dim-intersect? 1 2 0 2)) ;; .XX. vs XX
(assert (one-dim-intersect? 1 2 0 4)) ;; .XX. vs XXXX
(assert (one-dim-intersect? 0 4 1 2)) ;; XXXX vs .XX.

(defn- -intersection?
  "fonction d'aide pour intersection"
  [[x y w h] [x' y' w' h']]
  (and (one-dim-intersect? x w x' w') ;; Pour s'intersecter il faut avoir des x communs
       (one-dim-intersect? y h y' h'))) ;; et en même temps des y communs

(defn intersection?
  "Est-ce que ces commandes s'intersectent ?"
  [cmd1 cmd2]
  (let [f #(let [{ w :width h :height {x :x y :y} :corner} %]
             [x y w h])
        s1 (f cmd1) s2 (f cmd2)] ;; juste un parsing de la commande en vecteur
    (when (-intersection? s1 s2)
      [cmd1 cmd2]))) ;; on retourne la paire qui s'intersecte si ça marche, nil sinon

(assert (not (intersection? (parse-fn "#1 @ 1,3: 4x4") (parse-fn "#1 @ 8,8: 4x5"))))
(assert (intersection? (parse-fn "#1 @ 1,3: 4x4") (parse-fn "#1 @ 4,6: 4x5")))
(assert (intersection? (parse-fn "#1 @ 4,6: 4x5") (parse-fn "#1 @ 1,3: 4x4")))
(assert (intersection? (parse-fn "#1 @ 1,3: 4x4") (parse-fn "#1 @ 2,3: 4x5")))

(defn pas-de-recheche
  "Ajoute une nouvelle commande à la situation actuelle.
  Si elle intersecte un (ou plusieurs !) candidats tous vont dans les déchets.
  Sinon si elle intersecte un déchet elle les rejoints.
  Sinon elle reste dans les candidats"
  [[dechets candidats] nouveau]
  ;; Soient les candidats qui intersectent nouveau
  (let [ndechets (filter (partial intersection? nouveau) candidats)]
    (if (seq ndechets) ;; si on en trouve
        [(concat dechets ndechets [nouveau]) (remove (set ndechets) candidats)] ;; on les ajoute aux dechets
    (if (some #(intersection? % nouveau) dechets) ;; Sinon si on trouve une inter avec un dechet
      [(conj dechets nouveau) candidats] ;; on l'ajoute aussi au dechet
      [dechets (conj candidats nouveau)])))) ;; Sinon c'est un nouveau candidat

(defn solution2
  [input]
  (-> (transduce parse (completing pas-de-recheche) [[] []] (u/lines input))
      second
      first
      :id))

(time (solution2 input))
;; Remarques générales
;; La manière de gérer et chercher les intersections est sous-optimale.
;; Par exemple on pourrait construire un index sur x minimal :
;; si w' est  inférieur à tous les x_i rencontrés : pas d'intersection.
;; Sinon on limite le tests aux commandes de x_i inférieurs à w'.
;; C'est très rapide si on garde le tout dans un tableau indexé entre 0 et 1000
;; (la taille max de la pièce).
;; On peut faire pareil avec h_i, w_i et y_i, contre le coût de construire 4 tableaux.
