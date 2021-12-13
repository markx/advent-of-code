(ns day3
  (:require [clojure.string :as str]))



(def raw-input
  "
  00100
  11110
  10110
  10111
  10101
  01111
  00111
  11100
  10000
  11001
  00010
  01010
  ")

(comment
  (def raw-input
    (slurp "input-day3.txt")))


(def input
  (->> raw-input
    str/split-lines
    (remove str/blank?)
    (map str/trim)))

(defn gamma [input]
  (apply
    map
    (fn [& bits]
      (let [zeros (count (filter #{\0} bits))
            ones  (count (filter #{\1} bits))]
        (if (<= zeros  ones)
          ;; [gamma eplison]
          \1
          \0)))
    input))

(defn complement [bits]
  (for [b bits]
    (if (= \1 b)
      \0
      \1)))

(defn epsilon [input]
  (-> input
      gamma
      complement))


(defn part1 [input]
  (->> [(gamma input) (epsilon input)]
     (map #(Long/parseLong (apply str %) 2))
     (apply *)))


(defn oxygen [input]
  (loop [lines input
         i 0]
    (if (= 1 (count lines))
      (first lines)
      (let [g (first (gamma (map #(str (nth % i)) lines)))]
        (recur (filter #(= g (nth % i)) lines) (+ i 1))))))

(defn co2 [input]
  (loop [lines input
         i 0]
    (if (= 1 (count lines))
      (first lines)
      (let [g (first (epsilon (map #(str (nth % i)) lines)))]
        (recur (filter #(= g (nth % i)) lines) (+ i 1))))))


(defn part2 [input]
  (->> [(oxygen input) (co2 input)]
     (map #(Long/parseLong (apply str %) 2))
     (apply *)))



(comment
  (count (map (fn [& bits] (prn a) ) "00011" "11111"))
  (let [bits "00011"
        zeros (count (filter #{\0} bits))
        ones (count (filter #{\1} bits))]
    [zeros ones]))
