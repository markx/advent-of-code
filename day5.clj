(ns day5
  (:require [clojure.string :as str]))



(def raw-input
  "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(comment
  (def raw-input
    (slurp "input-day5.txt")))



(def input
  (->> raw-input
    str/split-lines
    (remove str/blank?)
    (map str/trim)))

(defn parse-line [line]
  (-> line
      (str/split #"\s+\->\s+")
      (->>
        (map #(str/split % #","))
        (map #(map parse-long %)))))

(defn points [[x1 y1] [x2 y2]]
  (if (and
        (not= x1 x2)
        (not= y1 y2))
    nil
    (if (= x1 x2)
      (for [y (range (min y1 y2) (+ 1 (max y1 y2)))]
        [x1 y])
      (for [x (range (min x1 x2) (+ 1 (max x1 x2)))]
        [x y1]))))

(defn abs [x]
  (if (< 0 x)
    x
    (- x)))

(defn points-2 [[x1 y1] [x2 y2]]
  (cond
    (= x1 x2)
    (for [y (range (min y1 y2) (+ 1 (max y1 y2)))]
      [x1 y])

    (= y1 y2)
    (for [x (range (min x1 x2) (+ 1 (max x1 x2)))]
      [x y1])

    :else
    (let [dir-x (if (< x1 x2) 1 -1)
          dir-y (if (< y1 y2) 1 -1)]
      (for [i (range (+ 1 (abs (- x1 x2))))]
        [(+ x1 (* dir-x i)) (+ y1 (* dir-y i))]))))

(comment
  (apply points (parse-line "1,1 -> 1,3"))
  (apply points (parse-line "9,7 -> 7,7"))
  (apply points (parse-line "0,0 -> 8,8"))
  (apply points-2 (parse-line "1,1 -> 3,3"))
  (apply points-2 (parse-line "9,7 -> 7,9")))

(defn part1 []
  (->> input
    (map parse-line)
    (mapcat #(apply points %))
    (frequencies)
    (filter #(< 1 (second %)))
    count))


(defn part2 []
  (->> input
    (map parse-line)
    (mapcat #(apply points-2 %))
    (frequencies)
    (filter #(< 1 (second %)))
    count))


(comment)




