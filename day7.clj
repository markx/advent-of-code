(ns day7
  (:require [clojure.string :as str]))



(def raw-input
  "16,1,2,0,4,2,7,1,2,14")


(comment)
(def raw-input
  (slurp "input-day7.txt"))


(def input
  (-> raw-input
    (str/trim)
    (str/split #",")
    (->>
      (map parse-long))))


(defn abs [x]
  (if (< 0 x)
    x
    (- x)))



(defn part1 []
  (let [mid (int (/ (count input) 2))
        median (nth (sort input) mid)]
    (->> input
      (map #(abs (- % median)))
      (reduce +))))


(defn fuel [x points]
  (let [ds (map #(abs (- x %)) points)]
    (reduce + (map #(/ (+ % (* % %)) 2) ds))))


(defn part2 []
  (let [left (reduce min input)
        right (reduce max input)]
    (->> (range left (inc right))
      (map #(fuel % input))
      (reduce min))))

