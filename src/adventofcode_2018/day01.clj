(ns adventofcode-2018.day01
  (:require [clojure.string :as str]))


;; part 1

(defn part1
  "Sums up all lines of input file"
  []
  (-> (slurp "./resources/day01/input.txt")
      (str/split #"\n")
      (->> (map #(Long/parseLong %))
           (reduce +))))


;; part 2

(defn first-repeat
  "Assuming the input sequence cycles back to a repeating value
  returns the first repeating value in a sequence."
  [coll]
  (loop [seen   #{} 
         coming coll]
    (if (seen (first coming))
      (first coming)
      (recur (conj seen (first coming))
             (rest coming)))))

(defn part2
  "Returns first repeating frequency"
  []
  (-> (slurp "./resources/day01/input.txt")
      (str/split #"\n")
      (->> (map #(Long/parseLong %))
           (cycle)
           (reductions + 0)
           (first-repeat))))
