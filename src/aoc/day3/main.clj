(ns aoc.day3.main
  (:require [aoc.utils :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn input->paths [input]
  ;; Converts the input string into a list of paths, which is a list of moves ;;
  (map #(str/split % #",") (str/split input #"\n")))

(defn path->moves [path]
  ;; Converts the given path into moves, with the structure {:direction string :len int} ;;
  (map (fn [cmd] {:direction (first cmd) :len (Integer/parseInt (subs cmd 1))}) path))

(defn move->pos [initial-pos {:keys [direction len]}]
  ;; Expands the given move into a list of positions {:x int :y int},
  ;; starting from the initial-pos, according to the given direction
  ;; Example: for initial-pos {:x 0 :y 0} and move {:direction R :len 5}, will return
  ;; ({:x 1 :y 0} {:x 2 :y 0} {:x 3 :y 0} {:x 4 :y 0} {:x 5 :y 0})
  (let [move (case direction
               \U #(update % :y inc)
               \R #(update % :x inc)
               \D #(update % :y dec)
               \L #(update % :x dec))]
    (take len (rest (iterate move initial-pos)))))

(defn moves->pos [moves]
  ;; Expands a list of moves into a list of positions
  (second
   (reduce
    (fn [[pos all-pos] move]
      (let [expanded-move (move->pos pos move)]
        [(last expanded-move) (concat all-pos expanded-move)]))
    [{:x 0 :y 0} []]
    moves)))

(defn manhattan-distance [{x1 :x y1 :y} {x2 :x y2 :y}]
  ;; Calculates the manhattan distance between two points
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn index-of [v coll]
  (first (first (filter (fn [[idx value]] (= value v)) (map-indexed vector coll)))))

(defn part-1 []
  (let [[path1 path2] (input->paths (slurp "src/aoc/day3/input"))
        positions-1 (set (moves->pos (path->moves path1)))
        positions-2 (set (moves->pos (path->moves path2)))
        intersections (set/intersection positions-1 positions-2)]
    (println (apply min (map #(manhattan-distance {:x 0 :y 0} %) intersections)))))

(defn part-2 []
  (let [[path1 path2] (input->paths (slurp "src/aoc/day3/input"))
        pos1 (moves->pos (path->moves path1))
        pos2 (moves->pos (path->moves path2))
        intersections (set/intersection (set pos1) (set pos2))
        intersections-with-steps (map (fn [intersection] (+ 2 (index-of intersection pos1) (index-of intersection pos2))) intersections)]
    (println (apply min intersections-with-steps))))

(defn -main [& args]
  (part-1)
  (part-2))
