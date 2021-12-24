(ns day15
  (:require [clojure.string :as str]))


(def raw-input
  "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")


(comment
  (def raw-input
    (slurp "input-day15.txt")))


(def input
  (vec
    (for [line (str/split-lines raw-input)]
      (vec
        (for [c line]
          (parse-long (str c)))))))


(defn neighbors [width height [r c]]
  (cond-> []
    (< 0 r)            (conj [(dec r) c])
    (< r (dec height)) (conj [(inc r) c])
    (< 0 c)            (conj [r (dec c)])
    (< c (dec width))  (conj [r (inc c)])))


(comment
  (neighbors 10 10 [1 1])
  (new-risk 8)
  (full-map [[8]])
  (full-map input))


(defn new-risk [risk]
  (if (= 9 risk)
    1
    (inc risk)))


(defn full-map [grid]
  (let [height  (count grid)
        width   (count (first grid))
        new-map (vec (repeat (* 5 height) (vec (repeat (* 5 width) 0))))]
    (->> (for [r (range height)
               c (range width)
               i (range 5)
               j (range 5)]
           [(+ r (* height i)) (+ c (* width j))])
      (reduce
        (fn [new-map [r c]]
          (cond
            (and
              (< r height)
              (< c width))
            (assoc-in new-map [r c] (get-in grid [r c]))

            (< r height)
            (assoc-in new-map [r c] (new-risk (get-in new-map [r (- c width)])))

            :else
            (assoc-in new-map [r c] (new-risk (get-in new-map [(- r height) c])))))
        new-map))))


(defn part1 [input]
  (let [grid   input
        height (count input)
        width  (count (first input))]
    ;; For Dijkstra's algorithm, use priorityQueue here, and stop at exit point.
    ;; No need to deplete queue.
    (loop [queue (conj (clojure.lang.PersistentQueue/EMPTY) [0 0])
           costs (-> (vec (repeat
                            height
                            (vec (repeat width Integer/MAX_VALUE))))
                   (assoc-in [0 0] 0))]
      (if (empty? queue)
        (last (last costs))
        (let [cur     (peek queue)
              updated (for [nei (neighbors width height cur)
                            :let [cost' (+ (get-in costs cur) (get-in grid nei))]
                            :when (< cost' (get-in costs nei))]
                        {:pos nei, :cost cost'})
              costs'  (reduce
                        (fn [costs' {:keys [pos cost]}]
                          (assoc-in costs' pos cost))
                        costs
                        updated)]
          (recur (into (pop queue) (map :pos updated)) costs'))))))


(defn part2 [input]
  (-> input
    full-map
    part1))


(comment
  (time (part1 input))
  (time (part2 input)))

