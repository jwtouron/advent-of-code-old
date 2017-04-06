(ns advent-of-code.util)

(defn read-input-lines
  [path]
  (-> path
      clojure.java.io/resource
      slurp
      clojure.string/split-lines))

(def transpose
  (partial apply mapv vector))

