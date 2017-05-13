(ns advent-of-code.year-2016.day-9
  (:require [advent-of-code.util :refer [read-input-lines]]))

(def input
  (read-input-lines "year2016-day9-input.txt"))

(def pattern #"[(](\d+)x(\d+)[)]")

(defn decompress [s]
  (let [m (re-matcher pattern s)]
    (loop [out (StringBuilder.)]
      (if-let [[_ c r] (re-find m)]
        (let [[c r] [(Integer/parseInt c) (Integer/parseInt r)]
              out (.append out (subs s (.regionStart m) (.start m)))
              out (.append out (apply str (repeat r (subs s (.end m) (+ (.end m) c)))))]
          (.region m (+ (.end m) c) (.regionEnd m))
          (recur out))
        (str (doto out (.append (subs s (.regionStart m) (.regionEnd m)))))))))

(clojure.test/deftest test-decompress
  (clojure.test/are [x y] (= x (decompress y))
    "ADVENT" "ADVENT"
    "ABBBBBC" "A(1x5)BC"
    "XYZXYZXYZ" "(3x3)XYZ"
    "ABCBCDEFEFG" "A(2x2)BCD(2x2)EFG"
    "(1x3)A" "(6x1)(1x3)A"
    "X(3x3)ABC(3x3)ABCY" "X(8x2)(3x3)ABCY"))

(declare decompress-length)

(defn decompress-length* [s ^java.util.regex.Matcher m c r]
  (let [out (- (.start m) (.regionStart m))
        cxr (subs s (.end m) (+ (.end m) c))
        out (+ out (* r (decompress-length cxr)))]
    (.region m (+ (.end m) c) (.regionEnd m))
    out))

(defn decompress-length [s]
  (let [m (re-matcher pattern s)]
    (loop [out 0]
      (if-let [[_ c r] (re-find m)]
        (recur (+ out (long (decompress-length* s m (Integer/parseInt c) (Integer/parseInt r)))))
        (+ out (- (.regionEnd m) (.regionStart m)))))))

(clojure.test/deftest test-decompress-length
  (clojure.test/are [x y] (= x (decompress-length y))
    (count "XYZXYZXYZ") "(3x3)XYZ"
    (count "XABCABCABCABCABCABCY") "X(8x2)(3x3)ABCY"
    241920 "(27x12)(20x12)(13x14)(7x10)(1x12)A"
    445 "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"
    1996002 "(9x999)(2x999)xx"))

(defn solve-part1 [] (.length ^String (decompress (first input))))
(defn solve-part2 [] (decompress-length (first input)))

(clojure.test/deftest test-solve
  (clojure.test/is (= (solve-part1) 152851))
  (clojure.test/is (= (solve-part2) 11797310782)))
