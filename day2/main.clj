(require '[clojure.string :as str])

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
                    [left right] (split-at idx3 result)
                    v1 (nth result idx1)
                    v2 (nth result idx2)
                    res (if (= operation 1) (+ v1 v2) (* v1 v2))]
                    (recur
                        (concat left [res] (rest right))
                        (+ pos 4)
                    )
                )
            )
        )
    )
)

(defn main []
    (def raw-input (str/split (slurp "input") #","))
    (def operations (map #(Integer/parseInt %) raw-input))

    (println (compute operations))
)

(main)