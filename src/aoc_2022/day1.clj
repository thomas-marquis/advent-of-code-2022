(ns aoc-2022.day1
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
