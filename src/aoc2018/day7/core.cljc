(ns aoc2018.day7.core
  (:require [aoc2018.util :as u]
            [clojure.set :as set]
            [com.rpl.specter :refer :all]
            [net.cgrand.xforms :as xf]))

(def input "resources/day7/input1")

(defn ligne-parser
  "Transforme une ligne d'entrée en une paire (X Y) avec X devant être finie
   avant Y."
  [s]
  (->> s
       (re-find #"Step (\w) must be finished before step (\w) can begin.")
       (drop 1)))

;; Pour la première fois on va utiliser la capacité de réduction de
;; u/parser pour transformer l'entrée en une map entre la tache et
;; les tâches dont elle dépend avant de commencer.
(def parse
  (u/parser ligne-parser
            (completing
             (fn [m [x y]]
               (-> m
                   (update :lettres conj x y)
                   (update-in [:adjacence y] (fnil conj #{}) x))))
            {:lettres #{}
             :adjacence {}}))

(defn etapes-possibles
  [etat etapes-faites]
  (letfn [(moins-etapes-faites [x] (set/difference x etapes-faites))]
    (->> (:lettres etat)
         moins-etapes-faites
         (filter #(->> [:adjacence %]
                       (get-in etat)
                       moins-etapes-faites
                       count
                       zero?)))))

(defn etape-suivante
  [etat etapes-faites]
  (->> (etapes-possibles etat etapes-faites) sort first))

(defn solution1
  [input]
  (let [etat (first (eduction parse (u/lines input)))]
    (->> (:lettres etat)
         (reduce (fn [[faits sortie] _]
                   (let [suiv (etape-suivante etat faits)]
                     [(conj faits suiv) (conj sortie suiv)]))
                 [#{} []])
         second
         (apply str))))


(defn step-time
  "Calcule le temps d'execution de chaque tache. A = 61, B = 62, …, Z = 86"
  [s]
  (+ 60
     (- (int (first s))
        (dec (int \A)))))


(defn simulation
  [etat]
  (letfn [(dispo? [[_ t]]
            (<= t 0))
          (exist? [[a  ]]
            (some? a))
          (taches [xs] (->> xs
                            (remove (comp #(< % 0) second))
                            (map first)
                            set))
          (tache-dispo? [en-cours fait restant]
            (->> (etapes-possibles etat fait)
                 (filter (set restant))
                 first))
          (pas-de-temps [taches] (transform [ALL LAST] dec taches))]
    (loop [restant (:lettres etat)
           en-cours (repeat 5 [nil 0])
           fait #{}
           t 0]
      (if (empty? restant)
        (+ t (apply max 0 (map second en-cours)))
        (if-let [tache (and (some dispo? en-cours)
                            (tache-dispo? en-cours fait restant))]
          (recur (remove #{tache} restant)
                 (setval [(filterer dispo?) FIRST]
                         [tache (step-time tache)]
                         en-cours)
                 fait t)
          (if (->> en-cours ;; si certaines des taches
                    (filter exist?) ;; existent
                    (some dispo?)) ;; et sont finies
            (recur restant
                   (setval [ALL dispo? exist?] [nil 0] en-cours) ;; on les relâche
                   (into fait (select [ALL dispo? exist? FIRST] en-cours))
                   t) ;; et on les marque faites
            (recur restant (pas-de-temps en-cours)
                   fait (inc t))))))))

(defn solution2
  [input]
  (let [etat (first (eduction parse (u/lines input)))]
    (simulation etat)))
