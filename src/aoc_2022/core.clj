(ns aoc-2022.core
  (:gen-class) 
  (:require [clojure.string :as str]))

(defn read-cal-data 
  []
   (map (fn [elf-cal] (map #(Integer. %)
                       (str/split elf-cal #"\n")))
    (str/split
    (slurp "resources/elve_calories") #"\n\n")))

(defn sum-vector 
  [vect]
  (reduce #(+ %1 %2) 0 vect))

(defn day1 
  []
  (println (sum-vector 
            (take 3
            (sort > (map sum-vector
                      (read-cal-data)))))))

(defn read-rps-strategy-data 
  []
  (map 
   (fn [x] (str/split x #" "))
   (str/split 
    (slurp "resources/rps_strategy") #"\n")))


(defn get-score 
  [p1 p2]
   "A => pierre
   B => papier
   C => siceaux
   X => p2 loose
   Y => draw
   Z => p2 win"
  (cond
    (and (= p1 "A") (= p2 "X")) {:result :p1
                                 :p1 (+ 6 3) :p2 3}
    (and (= p1 "B") (= p2 "Y")) {:result :draw
                                 :p1 (+ 3 2) :p2 (+ 3 2)}
    (and (= p1 "C") (= p2 "Z")) {:result :p2
                                 :p1 3 :p2 (+ 6 1)}

    (and (= p1 "A") (= p2 "Y")) {:result :draw
                                 :p1 (+ 3 1) :p2 (+ 3 1)}
    (and (= p1 "B") (= p2 "X")) {:result :p1
                                 :p1 (+ 6 2) :p2 1}

    (and (= p1 "A") (= p2 "Z")) {:result :p2
                                 :p1 1 :p2 (+ 6 2)}
    (and (= p1 "C") (= p2 "X")) {:result :p1
                                 :p1 (+ 6 3) :p2 2}

    (and (= p1 "B") (= p2 "Z")) {:result :p2
                                 :p1 2 :p2 (+ 6 3)}
    (and (= p1 "C") (= p2 "Y")) {:result :draw
                                 :p1 (+ 3 3) :p2 (+ 3 3)}
    :else (throw (Exception. (str "invalid combination: " p1 p2)))))

(defn day2 
  []
  (println (let [data (read-rps-strategy-data)]
                   (reduce +
                           0 
                           (map #(get (apply get-score %) :p2) data)))))

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

(defn -main
  [& args]
  (day4))
