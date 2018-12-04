(ns aoc2018.day4.core
  (:require [aoc2018.util :as u]
            [net.cgrand.xforms :as xf]))

(def input "resources/day4/input1")
(defn parse-int [n] (Integer/parseInt n))

;; Chaque ligne de l'entrée est décomposée en deux, la marque temporelle
;; entre crochets et l'action qui la suit. On va détailler comment parser chacune.
(defn parse-timestamp
  "Vu une marque temporelle dans une chaîne de caractère, la transforme en données"
  [s]
  (let [pattern #"\[(\d\d\d\d)-(\d\d)-(\d\d) (\d\d):(\d\d)\]"]
    (->> s
         (re-find pattern)
         (drop 1) ;; detail technique
         (map parse-int) ;; on tranduit tout en nombres
         (apply #(hash-map :year %1 :month %2 :day %3 :hour %4 :minute %5))))) ;; on en fait une structure
(assert (= (parse-timestamp "[1518-08-12 00:43]")
           {:year 1518 :month 8 :day 12 :hour 0 :minute 43}))

(defn parse-action
  "Vu une action dans une chaîne de caractère, la transforme en données"
  [s]
  (cond
    (= s "falls asleep") :fall-asleep
    (= s "wakes up") :wake-up
    :else (let [pattern #"Guard #(\d+) begins shift"
                guard-id-str (->> s (re-find pattern) second )]
            (when guard-id-str
              [:begin-shift (parse-int guard-id-str)]))))

(assert (= (parse-action "falls asleep") :fall-asleep))
(assert (= (parse-action "wakes up") :wake-up))
(assert (= (parse-action "Guard #1607 begins shift") [:begin-shift 1607]))
(assert (= (parse-action "yolo swag") nil))

;; OK maintenant on peut simplement décomposer notre entrée en un temps et une action
(defn parse-line
  [s]
  (let [pattern #"(\[.*\]) (.*)"
        [_ timestamp-str action-str] (re-find pattern s)]
    (assoc (parse-timestamp timestamp-str)
           :action (parse-action action-str))))

(parse-line "[1518-09-04 00:18] falls asleep")
(parse-line "[1518-11-22 00:02] Guard #1607 begins shift")
(parse-line "[1518-09-14 00:49] wakes up")

(def parse (u/parser parse-line))

;; Maintenant les logs donnés en entrée ne sont pas dans l'ordre.
;; Pour ça on va les trier par date. Clojure sait trier les vecteurs par
;; ordre lexicographique, c'est ce qu'on veut, on va mettre la date dans un vecteur.
(def get-vec-date
  (juxt :year :month :day :hour :minute))

(def date-sort (xf/sort-by get-vec-date))

;; OK on a enfin une entrée parsée, et dans l'ordre chronologique.
;; maintenant il faut traduire ça en temps de sommeil de chaque garde, chaque nuit.
;; Déjà on sait que les actions impliquent le dernier garde a avoir commencé sa rotation.
;; On peut partitionner le flux d'actions selon les gardes
(defn add-action
  [{actuel :actuel :as m} {action :action :as log}]
  (if (vector? action) ;; si l'action est un changement de garde elle a la forme [_ id]
    (assoc m :actuel (second action)) ;; dans ce cas on change juste le garde actuel
    (update m :gardes (fnil conj []) (assoc log :garde actuel)))) ;; sinon on ajoute cette action au garde actuel

(def actions-par-gardes
  (comp parse
        date-sort
        (xf/reduce (completing add-action) {:actuel nil :gardes []})
        (mapcat :gardes)))

;; Maintenant on a pour chaque garde sa liste d'action totale. On peut :
;; - la casser en service. Heureusement, les gardes ne s'endorment et ne se réveillent
;;     que durant l'heure de minuit, ce qui facilite le découpage.
;; - transformer ces services en minutes endormies, minutes éveillées.
;;   En fait seul les minutes endormies nous intéressent.
;;   On peut même remarquer que le garde commence et fini toujours son service éveillé
(def intervalles-sommeil
  (comp (map :minute)
        (xf/partition 2)))

(def casser-service
  (xf/by-key (juxt :year :month :day)
             (comp intervalles-sommeil
                   (xf/into []))))

(def sommeil-total-de-garde
  (comp (map (fn [[a b]] (- b a)))
        (xf/reduce +)))

(def minutes-freq
  (comp (xf/for [[a b] % i (range a b)] i)
        (xf/by-key identity xf/count)
        (xf/into {})))

(def services-par-gardes
  (comp actions-par-gardes
        (xf/by-key :garde (comp casser-service
                                ;; sommeil-total-de-garde
                                ;; minutes-freq
                                (mapcat second)
                                (xf/transjuxt {:sommeil sommeil-total-de-garde
                                               :freq minutes-freq})))))

;; finalement, à partir de services-par-gardes on obtient
;; pour chaque garde son nombre de minutes de sommeil total
;; et la frequence pour chaque heure dormie.
;; il ne reste qu'à trouver le garde qui dort le plus et la minute à laquelle
;; il est le plus souvent endormi
(defn strategie1
  [gardes]
  (let [garde (first (apply max-key (comp :sommeil val) gardes))
        freqs (get-in gardes [garde :freq])
        minute (first (apply max-key val freqs))]
    [garde minute]))

(defn solution1
  [input]
  (->> input
       u/lines
       (into {} services-par-gardes)
       strategie1
       (apply *)))

(solution1 input)


;; Pour la seconde stratégie on prend juste le garde qui dort le plus à une
;; minute précise, sans que ce soit le garde qui dorme le plus en général
(defn strategie2
  [gardes]
  (->> gardes
       (into {} (xf/by-key (map (comp #(apply max-key val %) :freq))))
       (apply max-key (fn [[garde [min freq]]] freq))
       flatten
       drop-last))

(defn solution2
  [input]
  (->> input
       u/lines
       (into {} services-par-gardes)
       strategie2
       (apply *)))

(solution2 input)
