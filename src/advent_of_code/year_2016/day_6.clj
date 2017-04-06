(ns advent-of-code.year-2016.day-6
  (:require [advent-of-code.util :refer [read-input-lines transpose]]))

(defonce input
  (read-input-lines "year2016-day6-input.txt"))

(defn least-common
  [seq]
  (let [fs (frequencies seq)
        f (fn [[x1 c1] [x2 c2]]
            (if (< c2 c1)
              [x2 c2]
              [x1 c1]))]
    (first (reduce f [nil (/ 1 0.0)] fs))))

(defn most-common
  [seq]
  (let [fs (frequencies seq)
        f (fn [[x1 c1] [x2 c2]]
            (if (> c2 c1)
              [x2 c2]
              [x1 c1]))]
    (first (reduce f [nil 0] fs))))

(defn solve-part1
  [input]
  (->> input
       transpose
       (map most-common)
       (apply str)))

(defn solve-part2
  [input]
  (->> input
       transpose
       (map least-common)
       (apply str)))
