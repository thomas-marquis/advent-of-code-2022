(ns aoc-2022.core
  (:gen-class) 
  (:require [clojure.string :as str]))

(require '[clojure.string :as str])

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
   X => p1 loose
   Y => draw
   Z => p1 win"
  (cond
    (and (= p1 "A") (= p2 "X")) {:result :draw
                                 :p1 (+ 3 1) :p2 (+ 3 1)}
    (and (= p1 "B") (= p2 "Y")) {:result :draw
                                 :p1 (+ 3 2) :p2 (+ 3 2)}
    (and (= p1 "C") (= p2 "Z")) {:result :draw
                                 :p1 (+ 3 3) :p2 (+ 3 3)}

    (and (= p1 "A") (= p2 "Y")) {:result :p2
                                 :p1 1 :p2 (+ 6 2)}
    (and (= p1 "B") (= p2 "X")) {:result :p1
                                 :p1 (+ 6 2) :p2 1}

    (and (= p1 "A") (= p2 "Z")) {:result :p1
                                 :p1 (+ 6 1) :p2 3}
    (and (= p1 "C") (= p2 "X")) {:result :p2
                                 :p1 3 :p2 (+ 6 1)}

    (and (= p1 "B") (= p2 "Z")) {:result :p2
                                 :p1 2 :p2 (+ 6 3)}
    (and (= p1 "C") (= p2 "Y")) {:result :p1
                                 :p1 (+ 6 3) :p2 2}
    :else (throw (Exception. (str "invalid combination: " p1 p2)))))

;; (defn get- [p1 p2]
;;   "X => loose
;;    Y => draw
;;    Z => win"
;;   (cond
;;     (and (= p1 "A") (= p2 "X")) {:result :draw
;;                                  :p1 (+ 3 1) :p2 (+ 3 1)}
;;     (and (= p1 "B") (= p2 "Y")) {:result :draw
;;                                  :p1 (+ 3 2) :p2 (+ 3 2)}
;;     (and (= p1 "C") (= p2 "Z")) {:result :draw
;;                                  :p1 (+ 3 3) :p2 (+ 3 3)}

;;     (and (= p1 "A") (= p2 "Y")) {:result :p2
;;                                  :p1 1 :p2 (+ 6 2)}
;;     (and (= p1 "B") (= p2 "X")) {:result :p1
;;                                  :p1 (+ 6 2) :p2 1}

;;     (and (= p1 "A") (= p2 "Z")) {:result :p1
;;                                  :p1 (+ 6 1) :p2 3}
;;     (and (= p1 "C") (= p2 "X")) {:result :p2
;;                                  :p1 3 :p2 (+ 6 1)}

;;     (and (= p1 "B") (= p2 "Z")) {:result :p2
;;                                  :p1 2 :p2 (+ 6 3)}
;;     (and (= p1 "C") (= p2 "Y")) {:result :p1
;;                                  :p1 (+ 6 3) :p2 2}
;;     :else (throw (Exception. (str "invalid combination: " p1 p2)))))

(defn day2 []
  (println (let [data (read-rps-strategy-data)]
                   (reduce +
                           0 
                           (map #(get (apply get-score %) :p2) data)))))

(defn -main
  [& args]
  (day2))
