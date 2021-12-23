(ns day12
  (:require [clojure.string :as str]
            [clojure.set :as s]))


(def raw-input
  "start-A
start-b
A-c
A-b
b-d
A-end
b-end")


(comment
  (def raw-input
    (slurp "input-day12.txt")))


(def input
  (->> raw-input
    (str/trim)
    (str/split-lines)
    (map #(str/split % #"-"))))


(defn graph [input]
  (reduce
    (fn [m [a b]]
      (-> m
        (update a conj b)
        (update b conj a)))
    {}
    input))


(defn small-cave? [s]
  (= s (str/lower-case s)))


(defn paths [graph node path visited]
  (if (= node "end")
    (list (conj path "end"))
    (let [path'     (conj path node)
          visited'  (if (small-cave? node)
                      (conj visited node)
                      visited)
          neighbors (for [neighbor (graph node)
                          :when (not (visited neighbor))]
                      neighbor)]
      (reduce
        (fn [results nei]
          (concat
            results
            (paths graph nei path' visited')))
        nil
        neighbors))))


(defn part1 [input]
  (-> input
    (graph)
    (paths "start" [] #{"start"})
    (count)))


(defn paths-2
  ([graph node]
   (paths-2 graph node [] #{node} nil))

  ([graph node path visited revisiting-cave]
   (if (= node "end")
     (when (or
             (nil? revisiting-cave)
             ;; If revisiting-cave is not visited twice,
             ;; it's a redundant as if no revisiting-cave was picked
             (= 2 ((frequencies path) revisiting-cave)))
       (list (conj path "end")))
     (let [path'     (conj path node)
           visited'  (if (small-cave? node)
                       (conj visited node)
                       visited)
           neighbors (for [neighbor (graph node)
                           :when (not (visited neighbor))]
                       neighbor)]
       (reduce
         (fn [results nei]
           (concat
             results
             (paths-2 graph nei path' visited' revisiting-cave)
             (when (and
                     (nil? revisiting-cave)
                     (small-cave? node))
               ;; Will revisit current node later
               (paths-2 graph nei path' visited node))))
         nil
         neighbors)))))


(defn part2 [input]
  (-> input
    (graph)
    (paths-2 "start")
    (count)))


(comment
  (part1 input)
  (part2 input))
