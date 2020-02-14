(ns aoc.day1.main
  (:require [aoc.utils :refer :all]
            [clojure.string :as str]))

(defn calculate-fuel [mass]
  (int (- (Math/floor (double (/ mass 3))) 2)))

(defn calculate-fuel-with-fuel [mass]
  (loop [current-fuel (calculate-fuel mass) sum 0]
    (if (<= current-fuel 0)
      sum
      (recur (calculate-fuel current-fuel) (+ sum current-fuel)))))

(defn -main [& args]
  (def raw-input (str/split-lines (slurp "src/aoc/day1/input")))
  (def module-masses (map #(Integer/parseInt %) raw-input))
  (def fuel-per-module (map calculate-fuel module-masses))
  (def fuel-per-module-with-fuel (map calculate-fuel-with-fuel module-masses))
  (def total-fuel (reduce + fuel-per-module))
  (def total-fuel-with-fuel (reduce + fuel-per-module-with-fuel))
  (println total-fuel)
  (println total-fuel-with-fuel))