(ns aoc-2022.day3
  (:require [clojure.string :as str]))

(defn load-rocksack-items
  []
  (str/split
   (slurp "resources/rucksack_items")
   #"\n"))

(defn get-rocksack-content
  [sack]
  (let [len (count sack)
        full-sack (map str sack)
        first-half (int (/ len 2))
        second-half (- len first-half)]
    {:first (into [] (take first-half full-sack))
     :last (into [] (take-last second-half full-sack))}))

(def item-types
  (str/split "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" #""))

(defn compute-item-priority
  [item]
  (+ 1
     (.indexOf item-types (str item))))

(defn get-first-duplicated-item
  [items1 items2]
  (loop [i 0]
    (let [item (get items1 i)
          size (count items1)]
      (if (or
           (some #(= item %) items2)
           (>= i size))
        item
        (recur (inc i))))))

(defn get-first-dupl-item-in-three
  [l1 l2 l3]
  (loop [i 0]
    (let [item (get l1 i)
          size (count l1)]
      (if (or
           (and
            (some #(= item %) l2)
            (some #(= item %) l3))
           (>= i size))
        (str item)
        (recur (inc i))))))

(defn get-all-dupl-priorities
  []
  (let [items (load-rocksack-items)]
    (map
     (fn [raw-content]
       (let [sack-content (get-rocksack-content raw-content)
             first-comp (get sack-content :first)
             last-comp (get sack-content :last)]
         (compute-item-priority
          (get-first-duplicated-item first-comp last-comp))))
     items)))

(defn get-group-of-three
  []
  (let [raw-data (map
                  #(into [] (map str %))
                  (load-rocksack-items))]
    (map
     #(into [] %)
     (partition 3 raw-data))))

(defn compute-groups-priorities
  []
  (let [groups (get-group-of-three)]
    (map
     (fn [group]
       (compute-item-priority
        (apply get-first-dupl-item-in-three group)))
     groups)))

(defn day3
  []
  (println (reduce + (compute-groups-priorities))))
