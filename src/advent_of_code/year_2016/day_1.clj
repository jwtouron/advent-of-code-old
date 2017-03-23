(ns advent-of-code.year-2016.day-1
  (:require [clojure.string :as str]))

(def input (as-> "L2, L5, L5, R5, L2, L4, R1, R1, L4, R2, R1, L1, L4, R1, L4, L4, R5, R3, R1, L1, R1, L5, L1, R5, L4, R2, L5, L3, L3, R3, L3, R4, R4, L2, L5, R1, R2, L2, L1, R3, R4, L193, R3, L5, R45, L1, R4, R79, L5, L5, R5, R1, L4, R3, R3, L4, R185, L5, L3, L1, R5, L2, R1, R3, R2, L3, L4, L2, R2, L3, L2, L2, L3, L5, R3, R4, L5, R1, R2, L2, R4, R3, L4, L3, L1, R3, R2, R1, R1, L3, R4, L5, R2, R1, R3, L3, L2, L2, R2, R1, R2, R3, L3, L3, R4, L4, R4, R4, R4, L3, L1, L2, R5, R2, R2, R2, L4, L3, L4, R4, L5, L4, R2, L4, L4, R4, R1, R5, L2, L4, L5, L3, L2, L4, L4, R3, L3, L4, R1, L2, R3, L2, R1, R2, R5, L4, L2, L1, L3, R2, R3, L2, L1, L5, L2, L1, R4"
               input
             (str/split input #", ")
             (map (comp vec next #(re-matches #"(L|R)(\d+)" %)) input)
             (map (fn [[dir blocks]]
                    [({"L" :l "R" :r} dir) (read-string blocks)])
                  input)))

(def start-state {:path [[0 0]], :dir :north})

(defn rotate
  [{:keys [dir] :as state} rot]
  (let [rotations {[:north :l] :west
                   [:west :l] :south
                   [:south :l] :east
                   [:east :l] :north
                   [:north :r] :east
                   [:west :r] :north
                   [:south :r] :west
                   [:east :r] :south}]
    (assoc state :dir (rotations [dir rot]))))

(defn move
  [{:keys [path dir] :as state} blocks]
  (let [[row col] (last path)
        build-path-vert  #(reduce (fn [path r] (conj path [r col])) path %)
        build-path-horiz #(reduce (fn [path c] (conj path [row c])) path %)
        new-path (case dir
                   :north (build-path-vert (range (dec row) (- row blocks 1) -1))
                   :south (build-path-vert (range (inc row) (+ row blocks 1)))
                   :east  (build-path-horiz (range (inc col) (+ col blocks 1)))
                   :west  (build-path-horiz (range (dec col) (- col blocks 1) -1)))]
    (assoc state :path new-path)))


(defn step
  [{:keys [pos dir] :as state} [rot blocks]]
  (-> state (rotate rot) (move blocks)))

(defn manhattan-distance
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn solve-part1
  []
  (let [{:keys [path]} (reduce step start-state input)]
    (manhattan-distance (last path) [0 0])))

(defn path [coords1 coords2]
  (let [[[x1 y1] [x2 y2]] (sort [coords1 coords2])]
    (if (= x1 x2)
      (reduce (fn []) [] (range y1 (inc y2))))))

(defn solve-part2 ;; 140
  []
  (let [{:keys [path]} (reduce step start-state input)
        twice-visited (reduce #(if (%1 %2) (reduced %2) (conj %1 %2)) #{} path)]
    (manhattan-distance twice-visited [0 0])))
