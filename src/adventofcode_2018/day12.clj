(ns adventofcode-2018.day12
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.core.matrix :as m]))

;;; part 1

(defn parse-rule
  [line]
  (let [rule-pattern #"(.{5}) => (.)" ]
    (rest (re-find rule-pattern line))))


(defn parse-data
  [filename]
  (let [;filename "./resources/day12/input.txt"
        input (-> (slurp filename)
                  (str/split #"\n"))
        initial-state (first (rest (re-find #"initial state: (.+)" (first input))))
        rule-pattern #"([\.\#]{5}) => (.)"
        rules (->> (drop 2 input)
                   (map parse-rule)
                   (flatten)
                   (apply hash-map))]
    [initial-state rules]))


(defn apply-rules
  [rules pattern]
  (if (contains? rules pattern)
    (rules pattern)
    "."))


(defn next-state
  [rules state]
  (let [new-line (apply str (concat "..." state "..."))]
    (loop [line new-line
           next-line []]
      (let [pattern (apply str (take 5 line))]
        (if (< (count line) 5)
          (apply str next-line)
          (recur (rest line)
                 (conj next-line (rules pattern))))))))


(defn count-plants
  [line epoch]
  (let [offset (- epoch)
        indexes (range offset (+ (count line) offset))]
    (->>(interleave indexes line)
        (partition 2)
        (filter #(= \# (second %)))
        (map first)
        (reduce +))))


(defn simulate-epoch
  [input final-epoch]
  (let [[initial-state rules] (parse-data input)
        step-epoch (partial next-state rules)
        initial-count (count initial-state)]
    (loop [epoch 0
           state initial-state]
      (if (= epoch final-epoch)
        state 
        (recur (inc epoch)
               (step-epoch (take (+ (* 2 epoch) initial-count) state)))))))

(defn part1
  [input epoch]
  (count-plants (simulate-epoch input epoch) epoch))

(time (println (part1 "./resources/day12/input.txt" 20)))
;;;3217
;;;"Elapsed time: 5.447573 msecs"


;;; part 2
    
(time (println (part1 "./resources/day12/input.txt" 5000)))
;;;400866
;;;"Elapsed time: 46601.037461 msecs" ...

(time (println (part1 "./resources/day12/input.txt" 5000000000)));; DNF...

