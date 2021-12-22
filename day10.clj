(ns day10
  (:require [clojure.string :as str]
            [clojure.set :as s]))


(def raw-input
  "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")


(comment
  (def raw-input
    (slurp "input-day10.txt")))


(def input
  (->> raw-input
    (str/trim)
    (str/split-lines)))


(def mapping {\( \), \[ \], \{ \}, \< \>})
(def scores {\) 3 , \] 57, \} 1197, \> 25137})


(defn  check-corrupted [line]
  (loop [i 0
         stack '()]
    (let [c (get line i)]
      (if (contains? #{\( \[ \{ \<} c)
        (recur (inc i) (conj stack c))
        (if (= (mapping (first stack)) c)
          (recur (inc i) (rest stack))
          c)))))


(defn  check-incomplete [line]
  (loop [i 0
         stack '()]
    (let [c (get line i)]
      (if (contains? #{\( \[ \{ \<} c)
        (recur (inc i) (conj stack c))
        (if (= (mapping (first stack)) c)
          (recur (inc i) (rest stack))
          (map mapping stack))))))


(defn scores2 [xs]
  (let [scores {\) 1 , \] 2, \} 3, \> 4}]
    (reduce
      (fn [acc c]
        (+
          (* acc 5)
          (scores c)))
      0
      xs)))


(defn part1 []
  (->> input
    (map check-corrupted)
    (filter some?)
    (map scores)
    (reduce +)))


(defn part2 []
  (->> input
    (remove check-corrupted)
    (map check-incomplete)
    (map scores2)
    sort
    (#(nth % (bit-shift-right (count %) 1)))))

