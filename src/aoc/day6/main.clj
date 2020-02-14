(ns aoc.day6.main
  (:require [aoc.utils :refer :all]
            [clojure.string :as str]))

(def map-test (mapv (fn [i] (str/split i #"\)")) ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L"] ))
(def orbits-map
  (mapv
    (fn [i](str/split i #"\)"))
    (-> "src/aoc/day6/input.txt" slurp (str/split #"\n"))))
(def map-objects (distinct (flatten orbits-map)))

(defn count-orbits [map start]
  (loop [count 0 pos start]
    (if (= pos "COM")
    count
    (recur (inc count) (first (first (filter (fn [i] (= (last i) pos)) map)))))))

(defn -main [& args]
  (println (reduce
    +
    (map
      (fn [i] (count-orbits orbits-map i))
      map-objects))))