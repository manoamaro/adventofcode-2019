(require '[clojure.string :as str])

(defn replace-at [coll i value]
  (let [[left right] (split-at i coll)]
    (concat left [value] (rest right))))

(defn to-int [s] (Integer/parseInt s))

(def input 1)

(def opcodes-fun {1 +
                  2 *
                  3 :input
                  4 :output
                  99 :exit})

(defn compute [operations]
  (let [operations-size (count operations)]
    (loop [result operations pos 0]
      (let [opcode (opcodes-fun (nth result pos))]
        (cond
          (= opcode :exit) result
          (= opcode +) (let [[idx1 idx2 idx3] (subvec result (inc pos) (+ pos 4))
                             v1 (nth result idx1)
                             v2 (nth result idx2)
                             res (+ v1 v2)] (recur (replace-at result idx3 res) (+ pos 4)))

          (= opcode *) (let [[idx1 idx2 idx3] (subvec result (inc pos) (+ pos 4))
                             v1 (nth result idx1)
                             v2 (nth result idx2)
                             res (* v1 v2)] (recur (replace-at result idx3 res) (+ pos 4)))

          (= opcode :input) (let [idx1 (nth result (inc pos))] (recur (replace-at result idx1 input) (+ pos 2)))
          (= opcode :output) (let [idx1 (nth result (inc pos))
                             v (nth result idx1)] (println v) (recur result (+ pos 2)))
          :else (let [opcode-str (str opcode)
                      opcode-len (count opcode-str)
                      new-opcode (to-int (subs opcode-str (- opcode-len 2)))
                      arg-type-1 (if (>= (- opcode-len 3) 0) (nth opcode-str (- opcode-len 3)) \0)
                      arg-type-2 (if (>= (- opcode-len 4) 0) (nth opcode-str (- opcode-len 4)) \0)
                      arg-type-3 (if (>= (- opcode-len 5) 0) (nth opcode-str (- opcode-len 5)) \0)]
                  (cond
                    (= new-opcode 1) (let [[idx1 idx2 idx3] (subvec result (inc pos) (+ pos 4))
                                           v1 (if (= arg-type-1 \1) idx1 (nth result idx1))
                                           v2 (if (= arg-type-2 \1) idx2 (nth result idx2))
                                           res-pos (if (= arg-type-3 \1) (+ pos 3) (nth result (+ pos 3)))
                                           res (+ v1 v2)]
                                       (recur (replace-at result res-pos res) (+ pos 4)))

                    (= new-opcode 2) (let [[idx1 idx2 idx3] (subvec result (inc pos) (+ pos 4))
                                           v1 (if (= arg-type-1 \1) idx1 (nth result idx1))
                                           v2 (if (= arg-type-2 \1) idx2 (nth result idx2))
                                           res-pos (if (= arg-type-3 \1) (+ pos 3) (nth result (+ pos 3)))
                                           res (* v1 v2)]
                                       (recur (replace-at result res-pos res) (+ pos 4)))
                    (= new-opcode 4) (let [idx1 (nth result (inc pos))
                                           v (if (= arg-type-1 \1) idx1 (nth result idx1))] (println v) (recur result (+ pos 2)))

                    :else (println opcode))))))))

(defn part1 []
  (def raw-input (str/split (slurp "input") #","))
  (def operations (map to-int raw-input))
  (compute operations))

(part1)

;(println (compute [3 0 4 0 99]))
;;(println (compute [1002 4 3 4 33]))