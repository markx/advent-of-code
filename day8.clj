(ns day8
  (:require [clojure.string :as str]
            [clojure.set :as s]))




(def raw-input
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")


(comment
  (def raw-input
    (slurp "input-day8.txt")))


(def input
  (->> raw-input
    (str/trim)
    (str/split-lines)))

(defn output [line]
  (-> line
    (str/split  #"\|")
    (second)
    (str/trim)
    (str/split  #"\s+")))


(defn part1 []
  (->> input
    (map output)
    (mapcat
      (fn [line]
        (map count line)))
    (frequencies)
    (filter (comp #{2 3 4 7} first))
    (map second)
    (reduce +)))


(defn unique [w]
  (condp = (count w)
    2 1
    4 4
    3 7
    7 8
    nil))

(defn non-unique [w uniques]
  (let [ws (set w)]
    (condp = (count w)
      5
      (cond
        (s/subset? (uniques 1) ws) 3
        (= 2 (count (s/intersection (uniques 4) ws))) 2
        :else 5)

      6
      (if (s/subset? (uniques 1) ws)
        (if (s/subset? (uniques 4) ws)
          9
          0)
        6)

      ; other
      nil)))



(defn decode [line]
  (let [words (-> line
                (str/replace #" \| " " ")
                (str/split  #"\s+"))
        m  (reduce
             (fn [acc w]
               (if-let [u (unique w)]
                 (assoc acc u (set w))
                 acc))
             {}
             words)
        m' (reduce
             (fn [acc w]
               (if-some [u (non-unique w m)]
                 (assoc acc u (set w))
                 acc))
             m
             words)]
   (reduce-kv
     (fn [acc k v]
      (assoc acc (str/join (sort (vec v))) k))
     {}
     m')))


(defn part2 []
  (->> input
    (map
      (fn [line]
        (let [code (decode line)
              o (output line)]
          (reduce
            (fn [number d]
              (+ (code (str/join (sort d)))
                 (* number 10)))
            0
            o))))
    (reduce +)))








