(ns day11
  (:require [clojure.string :as str]
            [clojure.set :as s]))


(def raw-input
  "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")


(comment)
(def raw-input
  (slurp "input-day11.txt"))


(def input
  (->> raw-input
    (str/trim)
    (str/split-lines)
    (mapv #(mapv (comp parse-long str) %))))


(defn neighbors [[r c]]
  (for [i (range -1 2)
        j (range -1 2)
        :when (not (= i j 0))]
    [(+ r i) (+ c j)]))


(defn bfs [grid queue flashing]
  (if (empty? queue)
    grid
    (let [cur (first queue)
          cur-val (get-in grid cur)]
      (cond
        (contains? flashing cur)
        (recur grid (rest queue) flashing)

        (< cur-val 9)
        (recur (update-in grid cur inc) (rest queue) flashing)

        :else
        (let [nei (for [n (neighbors cur)
                        :let [n-val (get-in grid n)]
                        :when (and
                                (some? n-val)
                                (not (contains? flashing n)))]
                    n)]
          (recur
            (assoc-in grid cur 0)
            (into queue nei)
            (conj flashing cur)))))))


(defn step [grid]
  (let [all-cords (for [i (range (count grid))
                        j (range (count (first grid)))]
                    [i j])]
    (bfs grid (vec all-cords) #{})))


(defn part1 [input n]
  (loop [i 0
         grid input
         total-flash 0]
    (if (= i n)
      total-flash
      (let [grid' (step grid)
            flash (count (filter #{0} (flatten grid')))]
        (recur (inc i) grid' (+ total-flash flash))))))


(defn part2 [input]
  (loop [i 0
         grid input]
    (if (every? #{0} (flatten grid))
      i
      (recur (inc i) (step grid)))))


(comment
  (part1 input 100)
  (part2 input))
