(ns adventofcode-2018.day05
  (:require [clojure.string :as str]))


;;; part 1


(defn inverted-polarity?
  [char-a char-b]
  (if (or (nil? char-a) (nil? char-b))
    false
      (let [case-diff (- (int \a) (int \A))
            a (int char-a)
            b (int char-b)]
        (or (= (- a b) case-diff)
            (= (- b a) case-diff)))))

(defn react
  [input]
  (loop [done    (vector (first input))
         polymer (rest input)]
    (if (empty? polymer)
      done
      (if (inverted-polarity? (peek done) (first polymer))
        (recur (pop done)
               (rest polymer))
        (recur (conj done (first polymer))
               (rest polymer))))))

(defn part1
  []
  (->> (slurp "./resources/day05/input.txt")
       (react)
       (str/join)
       (str/trim)
       (count)
       (println)))

(time (part1)) ;=>9288
;;;"Elapsed time: 19.177845 msecs"
;;;old version: "Elapsed time: 33002.850072 msecs" :/


;;; part 2

(defn removed-unit
  [input x]
  (let [c (Character/toLowerCase x)
        C (Character/toUpperCase x)]
    (->> input
         (remove #(= c %))
         (remove #(= C %)))))

(defn part2
  [input]
  (let [pre-removed-unit (partial removed-unit input)]
    (->> "abcdefghijklmnopqrstuvwxyz"
         (pmap #(count (str/trim (apply str (react (pre-removed-unit %))))))
         (apply min)
         (println))))


(time (part2 (slurp "./resources/day05/input.txt"))) ;=>5844
;;;"Elapsed time: 353.650712 msecs"
;;;old version: "Elapsed time: 440700.035086 msecs" :/
       
