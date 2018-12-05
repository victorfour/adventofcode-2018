(ns adventofcode-2018.day05
  (:require [clojure.string :as str]))


;;; part 1

(defn same-letter?
  [a b]
  (= (Character/toLowerCase b) (Character/toLowerCase a)))


(defn inverted-polarity?
  [a b]
  (if (or (nil? a) (nil? b))
    false
    (and (same-letter? a b)
         (or (and (Character/isLowerCase a)
                  (Character/isUpperCase b))
             (and (Character/isLowerCase b)
                  (Character/isUpperCase a))))))


(defn react
  [input]
  (loop [done    (vector (first input))
         polymer (rest input)]
    (if (empty? polymer)
      done
      (if (inverted-polarity? (last done) (first polymer))
        (recur (vec (drop-last done))
               (rest polymer))
        (recur (vec (conj done (first polymer)))
               (rest polymer))))))

(defn part1
  []
  (->> (slurp "./resources/day05/input.txt")
       (react)
       (apply str)
       (str/trim)
       (count)
       (println)))

;;;(time (part1)) ;=>9288
;;; "Elapsed time: 33002.850072 msecs" :/



;;; part 2

(def input (slurp "./resources/day05/input.txt"))

(defn removed-unit
  [x]
  (let [c (Character/toLowerCase x)
        C (Character/toUpperCase x)]
    (->> input ; sorry about that
         (remove #(= c %))
         (remove #(= C %)))))

(defn part2 []
  (->> "abcdefghijklmnopqrstuvwxyz"
       (pmap #(count (str/trim (apply str (react (removed-unit %))))))
       (apply min)
       (println)))


;;;(time (part2)) ;=>5844
;;; "Elapsed time: 440700.035086 msecs" :/
       
