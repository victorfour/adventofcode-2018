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
  (let [
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
  [rule-data state]
  (let [rules (partial apply-rules rule-data)
        new-line (apply str (concat "..." state "..."))]
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
           acc []
           state initial-state]
      ;(println state)
      (if (= epoch final-epoch)
        acc 
        (recur (inc epoch)
               (conj acc state)
               (step-epoch state))))))

(defn part1
  [input epoch]
  (count-plants (simulate-epoch input epoch) epoch))

(time (def thing (part1 "./resources/day12/input.txt" 96)))

;;;3217
;;;"Elapsed time: 5.447573 msecs"


;;; part 2

(defn trim-left
  [input]
  (loop [line input
         num-empty 0]
    (if (= \. (first line))
      (recur (rest line)
             (inc num-empty))
      [num-empty (apply str line)])))


(defn trim-right
  [input]
  (loop [line input
         num-empty 0]
    (if (= \. (last line))
      (recur (drop-last line)
             (inc num-empty))
      [num-empty (apply str line)])))


(defn trim-empty
  [input]
  (let [ltrim (trim-left input)
        rtrim (trim-right (second ltrim))]
    [[(first ltrim) (first rtrim)] (second rtrim)]))



(defn already-seen
  [history trimmed-state]
  (if ((:test-set history) (second trimmed-state))
    true
    false))


(defn update-history
  [history trimmed-state]
  (-> history
      (update :data #(conj % trimmed-state))
      (update :test-set #(conj % (second trimmed-state)))))


(defn find-repeating-pattern
  [input]
  (let [[initial-state rules] (parse-data input)
        step-epoch (partial next-state rules)
        initial-count (count initial-state)]
    (loop [history {:test-set #{} :data []}
           state initial-state]
      (let [trimmed-state (trim-empty state)]
        (if (already-seen history trimmed-state)
          (update-history history trimmed-state)
          (recur (update-history history trimmed-state)
                 (step-epoch state)))))))
    

(time (def example-history (find-repeating-pattern "./resources/day12/input.txt")))

(print (count (:data example-history)))

(pp/pprint example-history)
(pp/pprint (last (:data example-history)))
(def query (second (last (:data example-history))))

(println query)


(def history-data (:data example-history))

(println history-data)
(filter #(= query (second %)) history-data)

(println (count history-data))
(println history-data)

(def thing (simulate-epoch "./resources/day12/input.txt" 96))

(count-plants (nth thing 94) 94)
(println (last thing))

(println (second (trim-empty (last thing))))
(println (second (trim-empty (last (drop-last thing)))))
(= (second (trim-empty (last (drop-last thing)))) (second (trim-empty (last thing))))

(println (+ 8386 (* 80 (- 50000000000 94))))
;;; 4000000000866... Thank you repl.
