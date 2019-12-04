(require '[clojure.string :as str])

(defn replace-at [coll i value]
    (let [[left right] (split-at i coll)]
        (concat left [value] (rest right))
    )
)

(defn compute [operations]
    (let [operations-size (count operations)]
        (loop [result operations
            pos 0]
            (def operation (nth result pos))
            (if (or (= operation 99) (>= pos operations-size))
                result
                (let [
                    idx1 (nth result (+ pos 1))
                    idx2 (nth result (+ pos 2))
                    idx3 (nth result (+ pos 3))
                    v1 (nth result idx1)
                    v2 (nth result idx2)
                    res (if (= operation 1) (+ v1 v2) (* v1 v2))]
                    (recur
                        (replace-at result idx3 res)
                        (+ pos 4)
                    )
                )
            )
        )
    )
)

(defn part1 []
    (def raw-input (str/split (slurp "input") #","))
    (def operations (map #(Integer/parseInt %) raw-input))
    (def operations-modified (replace-at (replace-at operations 1 12) 2 2))
    (nth (compute operations-modified) 0)
)

(defn part2 []
    (def raw-input (str/split (slurp "input") #","))
    (def operations (map #(Integer/parseInt %) raw-input))
    (for [x (range 0 99)
          y (range 0 99)
          :let [
            operations-modified (replace-at (replace-at operations 1 x) 2 y)
            computed (compute operations-modified)
            result (nth computed 0)
          ]
          :when (= result 19690720)] (+ (* 100 x) y)
    )
)

(println (part1))
(println (part2))