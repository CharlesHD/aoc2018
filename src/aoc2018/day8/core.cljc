(ns aoc2018.day8.core
  (:require [aoc2018.util :as u]
            [clojure.set :as set]
            [com.rpl.specter :refer :all]
            [net.cgrand.xforms :as xf]))


(def input "resources/day8/input1")

(def parse (u/parser (fn [s] (->> (clojure.string/split s #" ")
                                   (map #(Integer/parseInt %))))))
;; Aujourd'hui on va parser un arbre. Youpi hourra.
;; Le premier exercice est donc principalement un exercice de
;; parenthèsage.
;; Une entrée du type
;; 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
;; devient
;; (2 3 (0 3 10 11 11) (1 1 (0 1 99) 2) 1 1 2)

;; Vu la nature des arbres, on doit pouvoir faire un truc assez recursif.

(defn -parse-arbre
  [nombres]
  ;; ok on extrait les deux premiers nombres de la suite
  (let [[nb-enfants nb-meta & reste] nombres]
    ;; maintenant on boucle sur le nombre d'enfants
    (loop [i nb-enfants
           nbrs reste
           etat {:enfants []
                 :metadata []}]
      (if (zero? i) ;; s'il ne reste aucun enfant à traiter
        (let [[meta xs] (split-at nb-meta nbrs)] ;; les nb-metas nombres suivant sont les metadonnées
          [(assoc etat :metadata meta) xs]) ;; on retourne l'arbre avec les nombres non traités.
        (let [[enfant reste] (-parse-arbre nbrs)] ;; Sinon on parse l'enfant suivant
          (recur (dec i)
                 reste
                 (update etat :enfants conj enfant))))))) ;; et on recommence #recursion

(defn parse-arbre
  [nombres]
  (first (-parse-arbre nombres)))

(def test-input [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

;; on peut définir un aggrégateur d'arbre rapidement
(defn aggreger-arbre
  [aggregator access-fn arbre]
  (apply aggregator
         (access-fn arbre)
         (map (partial aggreger-arbre aggregator access-fn) (:enfants arbre))))

;; on peut maintenant obtenir toutes les métadonnées en utilisant
;; la formule (aggreger-arbre concat :metadata arbre)
;; mais on peut aussi directement les sommer avec (aggreger-arbre + #(reduce + (:metadata %)) arbre)
(defn solution1
  [input]
  (let [arbre (->> input u/lines (into [] parse) first (parse-arbre))]
    (aggreger-arbre + #(reduce + (:metadata %)) arbre)))


;; la seconde partie demande de calculer la valeur d'un nœud.
;; 1. Un nœud sans enfant vaut la somme de ses métadonnées
;; 2. Un nœud avec enfants vaut la somme de la valeur de ses enfants
;;    indexés par ses metadonnées.
;;    Par exemple si le nœud A a pour metadonnées 1 2 et 100,
;;    v(A) = v(1er enfant de A) + v(2nd enfant de A) + v(100eme enfant de A).
;;    Si l'index pointe vers un enfant qui n'existe pas, sa valeur est de 0.

(defn valeur
  [arbre]
  (if (nil? arbre) ;; si l'arbre n'existe pas
    0 ;; sa valeur est de 0
    (if (seq (:enfants arbre)) ;; si l'arbre a des enfants
      (let [enfants-indexes (map #(nth (:enfants arbre) (dec %) nil) ;; on prend les enfants
                                 (:metadata arbre))] ;; qui sont indexes par les metadata
        (println enfants-indexes)
        (reduce + (map valeur enfants-indexes))) ;; et c'est la somme des valeurs de ces enfants
      ;; Sinon il s'agit de la somme de ses metadonnées
      (reduce + 0 (:metadata arbre)))))

(defn solution2
  [input]
  (let [arbre (->> input u/lines (into [] parse) first (parse-arbre))]
    (valeur arbre)))
