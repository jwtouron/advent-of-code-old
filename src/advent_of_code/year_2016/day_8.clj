(ns advent-of-code.year-2016.day-8
  (:require [advent-of-code.util :refer [read-input-lines transpose]]
            [clojure.core.match :refer [match]]))

(set! *warn-on-reflection* true)

(defn make-screen [width height]
  (vec (repeat height (vec (repeat width \.)))))

(defn draw-screen [screen]
  (apply str (interpose "\n" (map #(apply str %) screen))))

(def test-screen (make-screen 7 3))

(defonce input
  (read-input-lines "year2016-day8-input.txt"))

(def rect-pattern (re-pattern "rect (\\d+)x(\\d+)"))
(def rot-row-pattern (re-pattern "rotate row y=(\\d+) by (\\d+)"))
(def rot-col-pattern (re-pattern "rotate column x=(\\d+) by (\\d+)"))

(defn parse [^String line]
  (condp re-matches line
    rect-pattern :>> (fn [[_ width height]] [:rect (Integer/parseInt width) (Integer/parseInt height)])
    rot-row-pattern :>> (fn [[_ row by]] [:rotate-row (Integer/parseInt row) (Integer/parseInt by)])
    rot-col-pattern :>> (fn [[_ col by]] [:rotate-col (Integer/parseInt col) (Integer/parseInt by)])
    (throw (ex-info (str "Unparseable line: " line) {}))))

(defn turn-on-pixel [screen x y]
  (let [row (nth screen y)
        new-row (assoc row x \#)]
    (assoc screen y new-row)))

(defn rect [screen width height]
  (let [points (for [x (range width)
                     y (range height)]
                 [x y])]
    (reduce (fn [screen [x y]] (turn-on-pixel screen x y)) screen points)))

(defn rotate-row [screen row-num by]
  (let [row (nth screen row-num)]
    (assoc screen row-num (vec (take (count row) (drop (- (count row) by) (cycle row)))))))

(defn rotate-col [screen col by]
  (-> screen
      transpose
      (rotate-row col by)
      transpose))

(defn step [screen instr]
  (match instr
         [:rect width height] (rect screen width height)
         [:rotate-row row by] (rotate-row screen row by)
         [:rotate-col col by] (rotate-col screen col by)))

(defn num-lit-pixels
  [screen]
  (count (filter #(= \# %) (apply concat screen))))

(defn solve-part1 []
  (num-lit-pixels (reduce step (make-screen 50 6) (map parse input))))

(defn solve-part2 []
  (->> input
       (map parse)
       (reduce step (make-screen 50 6))
       draw-screen
       println))
