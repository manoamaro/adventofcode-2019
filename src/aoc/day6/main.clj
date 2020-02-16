(ns aoc.day6.main
  (:require [aoc.utils :refer :all]
            [clojure.string :as str]))

(def orbits-test1 (mapv (fn [i] (str/split i #"\)")) ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L"] ))
(def orbits-test2 (mapv (fn [i] (str/split i #"\)")) ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L" "K)YOU" "I)SAN"] ))

(def orbits-input
  (mapv
    (fn [i](str/split i #"\)"))
    (-> "src/aoc/day6/input.txt" slurp (str/split #"\n"))))

(defn group-orbits
  [orbits]
  (reduce (fn [dict [parent child]] (assoc dict child parent)) {} orbits))


(defn orbits-to-COM [orbits planet]
  (take-while #(not= "COM" %) (iterate orbits planet)))

(defn part1 []
  (let [orbits (group-orbits orbits-input)
        planets (keys orbits)]
        (reduce + (map (fn [i] (count (orbits-to-COM orbits i) )) (keys orbits)))))

(defn part2 []
  (let [orbits (group-orbits orbits-input)
        path-you (orbits-to-COM orbits "YOU")
        path-san (orbits-to-COM orbits "SAN")
        paths (concat path-you path-san)
        single-paths (filter #(= 1 (second %)) (frequencies paths))]
    (- (count single-paths) 2)))
        
(defn -main [& args]
  (println (part1))
  (println (part2)))