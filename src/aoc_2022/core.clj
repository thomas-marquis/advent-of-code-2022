(ns aoc-2022.core
  (:gen-class) 
  (:require [clojure.string :as str]))

(defn read-cal-data []
   (map (fn [elf-cal] (map #(Integer. %)
                       (str/split elf-cal #"\n")))
    (str/split
    (slurp "resources/elve_calories") #"\n\n")))

(defn sum-vector [vect]
  (reduce #(+ %1 %2) 0 vect))

(defn day1 []
  (println (sum-vector 
            (take 3
            (sort > (map sum-vector
                      (read-cal-data)))))))

(defn read-rps-strategy-data []
  (map 
   (fn [x] (str/split x #" "))
   (str/split 
    (slurp "resources/rps_strategy") #"\n")))


(defn get-score [p1 p2]
   "A => pierre
   B => papier
   C => siceaux
   X => p2 loose
   Y => draw
   Z => p1 win"
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

(defn day2 []
  (println (let [data (read-rps-strategy-data)]
                   (reduce +
                           0 
                           (map #(get (apply get-score %) :p2) data)))))

(defn load-rocksack-items []
   (str/split 
    (slurp "resources/rucksack_items") 
    #"\n"))

(defn get-rocksack-content [sack]
  (let [len (count sack)
        full-sack (map str sack)
        first-half (int (/ len 2))
        second-half (- len first-half)]
    {:first (into [] (take first-half full-sack))
     :last (into [] (take-last second-half full-sack))}))

(def item-types 
  (str/split "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" #""))

(defn compute-item-priority [item]
  (+ 1 
     (.indexOf item-types item)))

(defn get-duplicated-item [items1 items2]
  (loop [i 0]
    (let [item (get items1 i)
          size (count items1)]
      (if (or
           (some #(= item %) items2)
           (>= i size))
        item
        (recur (inc i))))))

(defn get-all-dupl-priorities []
  (let [items (load-rocksack-items)]
    (map
     (fn [sack-content]
       (let [first-comp (get sack-content :first)
             last-comp (get sack-content :last)]
         (compute-item-priority
          (get-duplicated-item first-comp last-comp))))
     (map get-rocksack-content items))))

(defn day3 []
  (println (reduce + (get-all-dupl-priorities))))

(defn -main
  [& args]
  (day3))
