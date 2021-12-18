(ns day6
  (:require [clojure.string :as str]))



(def raw-input
  "3,4,3,1,2")


(comment
  (def raw-input
    (slurp "input-day6.txt")))


(def input
  (-> raw-input
    (str/trim)
    (str/split #",")
    (->>
      (map parse-long))))

(comment)

(defn tick [input]
  (flatten (for [n input]
            (if (= n 0)
              [6 8]
              (dec n)))))

(defn part1 []
  (->> input
    (iterate tick)
    (#(nth % 80))
    (count)))


(defn tick2 [freqs]
  (reduce-kv
    (fn [freqs' age nums]
      (if (= age 0)
        (-> freqs'
          (update 6 (fnil + 0) nums)
          (assoc 8 nums))
        (update freqs' (dec age) (fnil + 0) nums)))
    {}
    freqs))

(defn part2 []
  (->> input
    (frequencies)
    (iterate tick2)
    (#(nth % 256))
    (vals)
    (apply +)))

