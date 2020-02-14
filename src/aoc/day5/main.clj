(ns aoc.day5.main
  (:require [aoc.utils :refer :all]
            [clojure.string :as str]))

(def input 5)

(def opcodes-fun {1 +
                  2 *
                  3 :input
                  4 :output
                  99 :exit})

(def modes-fn
  {0 (fn [program pos] (program pos))
   1 (fn [_program pos] pos)})

(defn parse-opcode [n]
  (let [[mode-3 mode-2 mode-1 op-a op-b] (take-last 5 (concat (repeat 4 0) (digits n)))
        modes [(modes-fn mode-1) (modes-fn mode-2) (modes-fn mode-3)]]
    {:modes modes
     :op (cond
           (= 1 op-b) :add
           (= 2 op-b) :mul
           (= 3 op-b) :put
           (= 4 op-b) :prn
           (= 5 op-b) :jump-if-true
           (= 6 op-b) :jump-if-false
           (= 7 op-b) :less-than
           (= 8 op-b) :equals
           (= [9 9] [op-a op-b]) :exit)}))

(def arg-count {:add 3 :mul 3 :put 1 :prn 1 :jump-if-true 2 :jump-if-false 2 :less-than 3 :equals 3 :exit 0})

(defn parse-instruction [program pos {:keys [op modes]}]
  (let [[f1 f2 _f3] modes
        [a1 a2 a3] (subvec program (inc pos) (+ (inc pos) (arg-count op)))
        new-pos (inc (+ pos (arg-count op)))]
    (case op
      :add
      (-> program
          (assoc a3 (+ (f1 program a1) (f2 program a2)))
          (vary-meta assoc :pos new-pos))
      :mul
      (-> program
          (assoc a3 (* (f1 program a1) (f2 program a2)))
          (vary-meta assoc :pos new-pos))
      :put
      (-> program
          (assoc a1 input)
          (vary-meta assoc :pos new-pos))
      :prn
      (do
        (prn (program a1))
        (vary-meta program assoc :pos new-pos))
      :jump-if-true
      (vary-meta program assoc :pos (if (not (= (f1 program a1) 0)) (f2 program a2) new-pos))
      :jump-if-false
      (vary-meta program assoc :pos (if (= (f1 program a1) 0) (f2 program a2) new-pos))
      :less-than
      (-> program
          (assoc a3 (if (< (f1 program a1) (f2 program a2)) 1 0))
          (vary-meta assoc :pos new-pos))
      :equals
      (-> program
          (assoc a3 (if (= (f1 program a1) (f2 program a2)) 1 0))
          (vary-meta assoc :pos new-pos))
      :exit
      program)))

(def values_ [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
              1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
              999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99])
(def raw-input (str/split (slurp "src/aoc/day5/input") #","))
(def values (mapv to-int raw-input))

(defn -main [& args]
  (loop [pos 0
         program values]
    (let [opcode (parse-opcode (program pos))]
      (when-not (= :exit (:op opcode))
        (let [program (parse-instruction program pos opcode)]
          (recur (:pos (meta program)) program))))))