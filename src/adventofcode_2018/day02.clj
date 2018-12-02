(ns adventofcode-2018.day02
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))


;;; part 1

(defn has-freq
  "1 if at least one frequency is `freq`
  0 otherwise"
  [freq x]
  (if (some #(= freq (second %)) x) 1 0))

(defn twice-and-thrice [x]
  [(has-freq 2 x)
   (has-freq 3 x)])

(defn part1 []
  (-> (slurp "./resources/day02/input.txt")
      (str/split #"\n")
      (->> (map frequencies)
           (map twice-and-thrice)
           (apply mapv vector)
           (map #(reduce + %))
           (reduce *))))


;;; part 2

(defn diff-by-one
  "assuming the two words have the same number of letters > 0
  returns true if only one letter is different"
  [[a b]]
  (->> (interleave a b)
       (partition 2)
       (map #(if (apply = %) 0 1))
       (reduce +)
       (= 1)))

(defn part2
  "assuming there are only two correct box IDs"
  []
  (-> (slurp "./resources/day02/input.txt")
      (str/split #"\n")
      (combo/combinations 2)
      (->> (filter diff-by-one)
           (flatten)
           (apply interleave)
           (partition 2)
           (filter #(apply = %))
           (map first)
           (apply str))))
