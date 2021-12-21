(ns day9
  (:require [clojure.string :as str]
            [clojure.set :as s]))


(def raw-input
  "2199943210
3987894921
9856789892
8767896789
9899965678")


(comment
  (def raw-input
    (slurp "input-day9.txt")))


(def input
  (->> raw-input
    (str/trim)
    (str/split-lines)
    (mapv #(mapv (comp parse-long str) %))))


(defn neighbors [[r c]]
  (list
    [(inc r) c]
    [(dec r) c]
    [r (inc c)]
    [r (dec c)]))


(defn low? [r c grid]
  (let [x (get-in grid [r c])]
    (->> (neighbors [r c])
      (map #(get-in grid %))
      (filter some?)
      (every? #(< x %)))))


(defn part1 []
  (->> (for [i (range (count input))
             j (range (count (first input)))
             :when (low? i j input)]
         (inc (get-in input [i j])))
    (reduce +)))


(defn bfs [grid queue visited result]
  (if (empty?  queue)
    result
    (let [start (first queue)
          x (get-in grid start)
          nei (for [n (neighbors start)
                    :let [nval (get-in grid n)]
                    :when (and
                            (some? nval)
                            (not (visited n))
                            (<= x nval)
                            (< nval 9))]
                n)
          queue' (concat (rest queue) nei)]

      (recur grid queue' (into visited nei) (conj result start)))))


(defn basin [input start]
  (bfs input [start] #{start} nil))


(defn part2 []
  (let [lows (->> (for [i (range (count input))
                        j (range (count (first input)))
                        :when (low? i j input)]
                    [i j]))]
    (->> lows
      (map #(basin input %))
      (map count)
      (sort)
      (take-last 3)
      (reduce *))))

