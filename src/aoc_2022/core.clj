(ns aoc-2022.core
  (:gen-class) 
  (:require [clojure.string :as str]))

(defn parse-procs
  [proc-lines]
  (map
   (fn [line]
     (let [[quantity from to] (rest 
                               (re-matches #"^move (\d+) from (\d+) to (\d+)$"
                                           line))]
       {:quantity (Integer. quantity)
        :from (Integer. from)
        :to (Integer. to)}))
   proc-lines))

(defn parse-init-ctrates-stacks
  [crates-lines]
  (let [col-numbers (map 
                     #(Integer. %)
                     (str/split
                      (str/replace (last crates-lines) #" " "")
                      #""))
        col-max (apply max col-numbers)
        stacks-matrix (into [] (map
                                (fn [line]
                                  (str/split line #""))
                                crates-lines))
        rows (map
                 (fn [line]
                   (map #(second %) (partition 4 line)))
                 (drop-last stacks-matrix))
        row-max (count rows)
        columns (map
                 (fn [col-idx]
                   (reverse 
                    (map
                     (fn [row-idx]
                       (nth
                        (nth rows row-idx)
                        col-idx))
                     (range row-max))))
                 (range col-max))]
    (map 
     (fn [col]
       (filter #(not-empty (str/replace % #" " "")) col))
     columns)))

(defn get-proc-start-index [lines]
  (let [lines-vect (into [] lines)
        lines-nb (count lines)]
    (loop [i 0]
      (let [line-value (get (into [] lines-vect) i)]
        (if (and
             (not= nil line-value)
             (or
              (str/starts-with? line-value "move")
              (>= i (- lines-nb 1))))
          i
          (recur (inc i)))))))

(defn load-file-content
  []
  (into [] (str/split
   (slurp "resources/crates_rearrangement") #"\n")))

(defn execute-part-1
  []
  (let [all-lines (take 20 (load-file-content))
        procs-start-index (get-proc-start-index all-lines)
        [crates-lines procs-lines] (split-at procs-start-index all-lines)
        procs (parse-procs procs-lines)
        crates (parse-init-ctrates-stacks
                (take (- procs-start-index 1) crates-lines))]
    [crates procs]))

(defn day5
  []
  (println (execute-part-1)))

(defn -main
  [& args]
  (day5))
