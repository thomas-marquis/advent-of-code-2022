(ns aoc-2022.core
  (:gen-class))

(require '[clojure.string :as str])

(defn read-data []
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
                      (read-data)))))))

(defn -main
  [& args]
  (day1))
