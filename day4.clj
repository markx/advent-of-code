(ns day4
  (:require [clojure.string :as str]))



(def raw-input
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1


22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7

  ")

(comment
  (def raw-input
    (slurp "input-day4.txt")))


(def numbers
  (->> raw-input
    str/split-lines
    first
    (#(str/split % #","))
    (map #(Integer/parseInt %))))

(def boards
  (->> raw-input
    str/split-lines
    (drop 1)
    (remove str/blank?)
    (partition 5)
    (map
      (fn [lines]
        (for [l lines]
           (->> (str/split l #"\s+")
              (remove str/blank?)
              (map #(Integer/parseInt %))))))))

(defn win? [board draw]
  (or
    (some
      #(every? draw %)
      board)
    (some true?
      (for [i (range (count (first board)))
            :let [col (map #(nth % i) board)]]
        (every? draw col)))))

(defn score [board draw last-draw]
  (*
    (apply + (remove draw (flatten board)))
    last-draw))



(defn part1 []
  (loop [i 0]
    (let [n (nth numbers i)
          draw (set (take (+ 1 i) numbers))
          winner (first (filter #(win? % draw) boards))]
      (if winner
        (score winner draw n)
        (recur (+ i 1))))))


(defn first-winner-i [board numbers]
  (first (filter #(win? board (set (take (+ 1 %) numbers))) (range (count numbers)))))

(defn part2 []
  (let [last-winner (apply max-key #(first-winner-i % numbers) boards)
        i (first-winner-i last-winner numbers)]
    (score last-winner (set (take (+ 1 i) numbers)) (nth numbers i))))


(comment
  (first-winner-i (nth boards 2) numbers))




