(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def input-min 387638)
(def input-max 919123)

(defn digits [n]
  ;; Converts a number n to a list of digits
  ;; 123 > [1 2 3]
  (->> n str (map (comp read-string str))))

(def pack
  ;; Pack consecutive duplicates of list elements into sublists
  (partial partition-by identity))

(defn check-n-1 [n]
  ;; Check the given number n if it fits the criteria for part 1
  (let [digits (digits n)
        sorted-digits (sort digits) ;;
        non-duplicated-digits (dedupe digits)]
    (and (= digits sorted-digits) (not= digits non-duplicated-digits))))

(defn check-n-2 [n]
  ;; Check the given number n if it fits the criteria for part 2
  (let [digits (digits n)
        sorted-digits (sort digits)
        non-duplicated-digits (dedupe digits)
        duplicated-count (count (filter #(= (count %) 2) (pack digits)))]
    (and (= digits sorted-digits) (not= digits non-duplicated-digits) (>= duplicated-count 1))))

(println (count (filter check-n-1 (range input-min input-max))))
(println (count (filter check-n-2 (range input-min input-max))))
