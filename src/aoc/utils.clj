(ns aoc.utils
  (:require [clojure.string :as str]))

(defn replace-at [coll i value]
  (let [[left right] (split-at i coll)]
    (concat left [value] (rest right))))

(defn to-int [s] (Integer/parseInt s))

(defn digits [n]
  ;; Converts a number n to a list of digits
  ;; 123 > [1 2 3]
  (->> n str (map (comp read-string str))))