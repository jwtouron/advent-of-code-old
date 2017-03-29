(ns advent-of-code.year-2016.day-4)

(def room-pattern
  (re-pattern (str "(.+)-" "(\\w+)" "\\[" "(\\w+)" "\\]")))

(def rooms
  (as-> (clojure.java.io/resource "year2016-day4-input.txt")
      it
    (slurp it)
    (clojure.string/split-lines it)
    (map #(as-> % it
            (re-matches room-pattern it)
            (rest it)
            (zipmap [:name :sector-id :checksum] it)
            (update-in it [:sector-id] read-string))
         it)))

(def char-counts
  (partial reduce
           (fn [m l]
             (update-in m [l] #(if (nil? %) 1 (inc %))))
           {}))

(defn most-common-letters
  [n letters]
  (as-> letters
      it
    (char-counts it)
    (seq it)
    (sort (fn [[c1 n1] [c2 n2]]
            (let [cmp (compare n1 n2)]
              (if (zero? cmp)
                (compare c1 c2)
                (* cmp -1))))
          it)
    (take n it)
    (map first it)))

(defn real-room?
  [{:keys [checksum name]}]
  (= (apply str (most-common-letters 5 (filter #(not= \- %) name)))
     checksum))

(defn solve-part1
  []
  (->> rooms
       (filter real-room?)
       (map :sector-id)
       (apply +)))

(defn real-room-test []
  (= (map real-room? [{:name "aaaaa-bbb-z-y-x" :checksum "abxyz"}
                      {:name "a-b-c-d-e-f-g-h" :checksum "abcde"}
                      {:name "not-a-real-room" :checksum "oarel"}
                      {:name "totally-real-room" :checksum "decoy"}])
     [true true true false]))

(def shift
  {\a \b
   \b \c
   \c \d
   \d \e
   \e \f
   \f \g
   \g \h
   \h \i
   \i \j
   \j \k
   \k \l
   \l \m
   \m \n
   \n \o
   \o \p
   \p \q
   \q \r
   \r \s
   \s \t
   \t \u
   \u \v
   \v \w
   \w \x
   \x \y
   \y \z
   \z \a})

(defn shift-char
  [n c]
  (if (shift c)
    (reduce (fn [c _] (shift c)) c (range n))
    c))

(defn shift-word
  [n w]
  (apply str (map #(shift-char n %) w)))

(defn solve-part2
  []
  (->> rooms
       (map (fn [{:keys [name sector-id]}] [(shift-word sector-id name) sector-id]))
       (filter (fn [[w s]] (when (re-matches #".*north.*" w) s)))
       (map #(nth % 1))
       first))
