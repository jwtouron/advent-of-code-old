(ns advent-of-code.year-2016.day-7
  (:require [advent-of-code.util :refer [read-input-lines]]
            [clojure.core.match :refer [match]]))

(set! *warn-on-reflection* true)

(defonce input
  (read-input-lines "year2016-day7-input.txt"))

(def pattern
  (re-pattern (str "([^\\[]*)" "(\\[" "([^\\[\\]]+)" "\\])?")))

(defn parse-line
  ([^String line]
   (parse-line line (re-matcher pattern line)))
  ([^String line ^java.util.regex.Matcher m]
   (let [[_ ^String out _ in] (re-find m)]
     (cond
       in (cons [:out out] (cons [:in in] (lazy-seq (parse-line nil m))))
       (and out (> (.length out) 0)) (cons [:out out] (lazy-seq (parse-line nil m)))
       :else nil))))

(defn abba? [^String s]
  (and (= (.length s) 4)
       (= (.charAt s 0) (.charAt s 3))
       (= (.charAt s 1) (.charAt s 2))
       (not= (.charAt s 0) (.charAt s 1))))

(defn contains-abba?
  [^String s]
  (some (comp abba? #(apply str %)) (partition 4 1 s)))

(defn supports-tls?
  [^String s]
  (let [f (fn [state [in-out s]]
            (match [state in-out (contains-abba? s)]
                   [:maybe :out true] :yes
                   [:yes :in true] (reduced :no)
                   [:maybe :in true] (reduced :no)
                   :else state))
        supports? (->> s
                    (parse-line)
                    (reduce f :maybe))]
    (not (#{:maybe :no} supports?))))

(defn test-part1
  []
  (doseq [[x y] [["abba[mnop]qrst" true]
                 ["abcd[bddb]xyyx" false]
                 ["aaaa[qwer]tyui" false]
                 ["ioxxoj[asdfgh]zxcvbn" true]]]
    (when (not= (supports-tls? x) y)
      (println "Wrong!:" x))))

(defn solve-part1
  []
  (->> input
       (filter supports-tls?)
       count))

(defn aba? [^String s]
  (and (= (.length s) 3)
       (= (.charAt s 0) (.charAt s 2))
       (not= (.charAt s 0) (.charAt s 1))))

(defn find-abas
  [^String s]
  (->> s
   (partition 3 1)
   (map #(apply str %))
   (filter aba?)
   set))

(defn invert
  [^String s]
  (str (.charAt s 1) (.charAt s 0) (.charAt s 1)))

(defn supports-ssl?
  [^String s]
  (let [f (fn [state [in-out s]]
            (match [in-out (find-abas s)]
                   [:out abas] (update state :abas clojure.set/union abas)
                   [:in babs] (update state :babs clojure.set/union babs)))]
    (as-> s
        it
      (parse-line it)
      (reduce f {:abas #{} :babs #{}} it)
      (if (some (set (map invert (it :babs))) (it :abas))
        true
        false))))

(clojure.test/deftest test-supports-ssl?
  (clojure.test/are [x y] (= x (supports-ssl? y))
    true "aba[bab]xyz"
    false "xyx[xyx]xyx"
    true "aaa[kek]eke"
    true "zazbz[bzb]cdb"))

(defn solve-part2
  []
  (->> input
       (filter supports-ssl?)
       count))
