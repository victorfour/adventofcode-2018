(ns adventofcode-2018.day08
  (:require [clojure.string :as str]))

;;; part 1

(def exampletxt "./resources/day08/example.txt")
(def inputtxt "./resources/day08/input.txt")
(def one-nodetxt "./resources/day08/one-node.txt")

(defn parse-data
  [filename]
  (-> (slurp filename)
      (str/trim)
      (str/split #" ")
      (->> (map #(Long/parseLong %)))))


(defn part1
  [input]
  (let [num-children (first input)
        num-elems (second input)
        tail (drop 2 input)]
    (loop [n-child num-children
           cur-input tail
           acc 0]
      (if (zero? n-child)
        [(drop num-elems cur-input) (+ acc (reduce + (take num-elems cur-input)))]
        (let [[next-input next-acc] (part1 cur-input)]
          (recur (dec n-child)
                 next-input
                 (+ acc next-acc)))))))

(println (part1 (parse-data inputtxt)))
;;=>[() 47112]
