(ns advent-of-code.year-2016.day-10
  (:require [advent-of-code.util :refer [read-input-lines]]
            [clojure.core.match :refer [match]]))

(def input
  (read-input-lines "year2016-day10-input.txt"))

(defn- parse
  [^String line]
  (condp re-matches line
    #"(value \d+) goes to (bot \d+)"
    :>> (fn [m] (apply vector :goes (rest m)))
    #"(bot \d+) gives low to ((?:bot|output) \d+) and high to ((?:bot|output) \d+)"
    :>> (fn [m] (apply vector :gives (rest m)))))

(defn- forward
  [factory bot-name]
  (let [bot (factory bot-name)]
    (if-not (= (count (bot :chips)) 2)
      factory
      (let [instr (bot :instr)
            [low high] (sort (bot :chips))]
        ))))

(defn- give
  [factory val bot-name]
  (update-in factory [bot-name :chips] conj val))

(defn- execute-instr
  [factory instr]
  (match instr
    [:goes val bot] (-> factory (give val bot) (forward bot))
    [:gives src low high] nil))

(defn solve-part1
  []
  (let [factory {}]
    (->> input
         (map parse)
         (reduce execute-instr factory))))


;;(map parse (take 20 input))
;;(parse (first input))
