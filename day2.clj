(ns day2
  (:require [clojure.string :as str]))



(def raw-input
  "
  forward 5
  down 5
  forward 8
  up 3
  down 8
  forward 2")

(def raw-input
  (->>
    (slurp "input-day2.txt")))


(def input
  (->> raw-input
    str/split-lines
    (remove str/blank?)
    (map str/trim)
    (map #(str/split % #"\s"))
    (map (fn [[cmd v]]
          [cmd (Integer/parseInt v)]))))


(defn apply-cmd [[pos depth]
                 [cmd v]]
    (case cmd
      "forward" [(+ v pos) depth]
      "up"      [pos (- depth v)]
      "down"    [pos (+ depth v)]))



(defn apply-cmd2 [[pos depth aim]
                  [cmd v]]
    (case cmd
      "forward" [(+ v pos) (+ depth (* aim v)) aim]
      "up"      [pos depth (- aim v)]
      "down"    [pos depth (+ aim v)]))

(defn part1 []
  (->> input
    (reduce
      apply-cmd
      [0 0])
    (reduce *)))


(defn part2 []
  (->> input
    (reduce
      apply-cmd2
      [0 0 0])
    (apply (fn [pos depth _]
             (* pos depth)))))


