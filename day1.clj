(ns day1)


(def input
  (->>
    (slurp "input-day1.txt")
    clojure.string/split-lines
    (map #(Integer/parseInt %))))

(defn part1 []
  (->> input
    (#(list % (next %)))
    (apply map #(- %2 %1))
    (filter #(< 0 %))
    (count)))


(defn part2 []
  (->> input
    (#(list % (next %) (next (next %))))
    (apply map +)
    (#(list % (next %)))
    (apply map #(- %2 %1))
    (filter #(< 0 %))
    (count)))

(comment)
