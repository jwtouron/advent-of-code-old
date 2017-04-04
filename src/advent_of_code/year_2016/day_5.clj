(ns advent-of-code.year-2016.day-5)

(defn md5 [^String x]
  (-> (java.security.MessageDigest/getInstance "MD5")
      (.digest (.getBytes x))
      (javax.xml.bind.DatatypeConverter/printHexBinary)))

(def password-pattern-part1
  (re-pattern "00000(\\w).*"))

(def password-pattern-part2
  (re-pattern "00000(\\d)(\\w).*"))

(def input "reyedfim")

(defn solve-part1
  []
  (let [f (fn [s n]
            (if (>= (count s) 8)
              (reduced (persistent! s))
              (if-let [[_ c] (re-matches password-pattern-part1 (md5 (str input n)))]
                (conj! s c)
                s)))]
    (apply str (reduce f (transient []) (range)))))

(defn solve-part2
  []
  (let [matches (for [i (range)
                      :let [[_ p c] (re-matches password-pattern-part2 (md5 (str input i)))]
                      :when p
                      :let [p (Long/parseLong p 16)]
                      :when (< -1 p 8)]
                  [p c])
        f (fn [s [p c]]
            (if (>= (count s) 8)
              (reduced (persistent! s))
              (if (s p)
                s
                (assoc! s p c))))
        chars (reduce f (transient {}) matches)]
    (->> (reduce #(conj %1 (chars %2)) [] (range 8))
         (apply str))))
