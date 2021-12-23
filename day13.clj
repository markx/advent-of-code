(ns day13
  (:require [clojure.string :as str]
            [clojure.set :as s]))


(def raw-input
  "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")


(comment
  (def raw-input
    (slurp "input-day13.txt")))


(def input
  (let [[dots folds] (-> raw-input
                       (str/trim)
                       (str/split #"\n\n"))
        dots (for [line (str/split-lines dots)]
               (-> line
                 (str/split #",")
                 (->>
                   (mapv parse-long))))
        folds (for [line (str/split-lines folds)
                    :let [[axis v] (next (re-matches #"fold along (\w)=(\d+)" line))]]
                [axis (parse-long v)])]
    [dots folds]))


(defn fold [dots [axis v]]
  (set
    (for [[x y] dots]
      (if (= axis "y")
        (cond
          (< y v)
          [x y]

          (= y v)
          nil

          :else
          [x (- v (- y v))])
        (cond
          (< x v)
          [x y]

          (= x v)
          nil

          :else
          [(- v (- x v)) y])))))


(defn part1 [input]
  (let [[dots folds] input]
    (-> (fold dots (first folds))
      count)))


(defn display [dots]
  (let [width (inc (reduce max (map first dots)))
        height (inc (reduce max (map second dots)))
        screen (vec (repeat height (vec (repeat width " "))))
        screen (reduce
                 (fn [screen' dot]
                   (assoc-in screen' (reverse dot) "#"))
                 screen
                 dots)]
    (doseq [line (mapv str/join screen)]
      (prn line))))


(defn part2 [input]
  (let [[dots folds] input
        result (reduce
                 (fn [dots' f]
                   (fold dots' f))
                 dots
                 folds)]
    (-> result
      display)))


(comment
  (part1 input)
  (part2 input))
