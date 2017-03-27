(ns advent-of-code.year-2016.day-3)

(def input
  (as-> (clojure.java.io/resource "year2016-day3-input.txt")
      it
    (slurp it)
    (clojure.string/split-lines it)
    (map #(->> %
               (re-matches #"\s*(\d+)\s+(\d+)\s+(\d+)\s*")
               rest
               (map read-string)
               vec)
         it)))


(defn triangle?
  [[a b c]]
  (and (> (+ a b) c)
       (> (+ b c) a)
       (> (+ a c) b)))


(defn solve-part1
  []
  (count (filter triangle? input)))

(defn solve-part2
  []
  (let [new-input (->> input (apply mapv vector) (apply concat) (partition 3) (map (partial into [])))]
    (count (filter triangle? new-input))))

