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
                   (map
                    (fn [row-idx]
                      (nth
                       (nth rows row-idx)
                       col-idx))
                    (range row-max)))
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

(defn move-one
  [proc stacks]
  (let [crate-to-move (first (nth stacks 
                                  (- (get proc :from) 1)
                                  nil))
        moved-stacks (map-indexed
                       (fn [idx stack]
                         (if (= (+ 1 idx) (get proc :from))
                           (rest stack)
                           (if (= (+ 1 idx) (get proc :to))
                             (conj stack crate-to-move)
                             stack)))
                       stacks)]
    (if (and 
         (not= nil crate-to-move)
         (not= (get proc :from)
               (get proc :to)))
      moved-stacks
      stacks)))

(defn move-n
  [stacks proc n]
  (if (> n 0)
    (recur
     (move-one proc stacks)
     proc
     (dec n))
    stacks))

(defn move
  [stacks procs idx]
  (let [proc (get (into [] procs) idx)]
    (if (= nil proc)
      stacks
      (recur (move-n stacks
                     proc
                     (get proc :quantity))
             procs
             (inc idx)))))

(defn get-tops
  [crates]
  (str/join "" (map #(first %) crates)))

(defn execute-part-1
  []
  (let [all-lines (take 320 (load-file-content))
        procs-start-index (get-proc-start-index all-lines)
        [crates-lines procs-lines] (split-at procs-start-index all-lines)
        procs (parse-procs procs-lines)
        crates (parse-init-ctrates-stacks
                (take (- procs-start-index 1) crates-lines))]
    (get-tops 
     (move crates procs 0))))

(defn day5
  []
  (println (execute-part-1)))
  ;; (println (move-n (list (list "D" "C" "B" "A")
  ;;                        (list "X"))
  ;;                  {:from 1 :to 2}
  ;;                  3)))

(defn -main
  [& args]
  (day5))
