(ns aoc-2022.day4
  (:require [clojure.string :as str]))

(defn load-cleanup-pairs-data
  []
  (map
   (fn [raw-pair]
     (let [raw-pair-vect (into []
                               (str/split raw-pair #","))]
       (map
        (fn [range-str]
          (map
           #(Integer. %)
           (into [] (str/split range-str #"-"))))
        (into [] raw-pair-vect))))
   (str/split (slurp "resources/cleanup_pairs")
              #"\n")))

(defn partially-overlap?
  [vect other]
  (let [vect-start (get (into [] vect) 0)
        vect-end (get (into [] vect) 1)
        other-start (get (into [] other) 0)
        other-end (get (into [] other) 1)]
    (or
     (and
      (>= vect-start other-start)
      (<= vect-start other-end))
     (and
      (<= vect-end other-end)
      (>= vect-end other-start)))))

(defn fully-overlap
  "vect is fully included in other ?
  Examples: vect = [3 5] ; other = [2 7] ; result = true"
  [vect other]
  (let [vect-start (get (into [] vect) 0)
        vect-end (get (into [] vect) 1)
        other-start (get (into [] other) 0)
        other-end (get (into [] other) 1)]
    (and
     (>= vect-start other-start)
     (<= vect-end other-end))))

(defn get-all-included
  [data]
  (map
   (fn [pair]
     (let [pair-vect (into [] pair)]
       (if (or (apply partially-overlap? pair)
               (partially-overlap? (get pair-vect 1)
                                   (get pair-vect 0)))
         1
         0)))
   data))

(defn day4
  []
  (println (reduce + (get-all-included (load-cleanup-pairs-data)))))
