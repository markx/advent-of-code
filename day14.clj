(ns day14
  (:require [clojure.string :as str]))



(def raw-input
  "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(comment
  (def raw-input
    (slurp "input-day14.txt")))


(defn parse-line [line]
  (-> line
      (str/split #"\s+\->\s+")))

(def input
  (let [[template rules] (str/split raw-input #"\n" 2)]
    {:template template
     :rules (-> rules
                (str/trim)
                (str/split-lines)
                (->>
                  (mapcat parse-line)
                  (apply hash-map)))}))

(defn pairs [template]
  (map str template (rest template)))

(defn merge [pairs]
  (reduce
    (fn [a b]
      (str a (apply str (rest b))))
    pairs))

(defn insert [pair rules]
  (when-let [v (get rules pair)]
    (str (first pair) v (second pair))))


(defn step [template rules]
  (->> template
    (pairs)
    (map #(insert % rules))
    (merge)))


(comment
  (insert "NN" (:rules input))
  (merge (pairs "NCB"))
  (get (hash-map "CH" "B") "CH")
  (get (:rules input) "CH"))



(defn part1 []
  (let [{template :template
         rules :rules} input
        result (nth (iterate #(step % rules) template) 10)]
    (->> result
      (frequencies)
      (map second)
      (sort)
      (#(- (last %) (first %))))))



(defn step-2 [rules freqs]
  (apply merge-with +
         (flatten
           (for [[k n] freqs
                 :let [a (first k)
                       b (second k)]]
             (if-let [v (get rules k)]
               [{(str a v) n (str v b) n}]
               {k n})))))


(defn count-char [a b freqs]
   (let [c-freqs (->> (for [[k n] freqs
                             :let [a (first k)
                                   b (second k)]]
                        [{a n} {b n}])
                   flatten
                   (apply merge-with +))]
     (-> c-freqs
      (update a #(+ 1 %))
      (update b #(+ 1 %))
      (->>
       (map (fn [[k v]] [k (/ v 2)]))))))



(defn part2 []
  (let [{template :template
         rules :rules} input]
    (->> template
      (pairs)
      (frequencies)
      (iterate #(step-2 rules %))
      (#(nth % 40))
      (count-char (first template) (last template))
      (map second)
      (sort)
      (#(- (last %) (first %))))))

