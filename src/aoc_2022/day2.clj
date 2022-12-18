(ns aoc-2022.day2
  (:require [clojure.string :as str]))

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
