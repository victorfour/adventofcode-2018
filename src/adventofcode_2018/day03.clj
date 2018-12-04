(ns adventofcode-2018.day03
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))


;;; part 1

(defn parse-claim
  "Returns a map of {:left :top :width :height} given a claim"
  [claim-line]
  (let [id (-> claim-line
               (str/split #"@")
               (first)
               (str/trim))
        [margin size] (-> claim-line
                          (str/split #"@")
                          (second)
                          (str/split #":")
                          (->> (map str/trim)
                             (vec)))
        [left top] (str/split margin #",")
        [width height] (str/split size #"x")]
    {:id     id
     :left   (Long/parseLong left)
     :top    (Long/parseLong top)
     :width  (Long/parseLong width)
     :height (Long/parseLong height)}))
  
(defn inside-1d?
  [index margin extent]
  (if (and (>= index margin)
           (< index (+ margin extent)))
    true
    false))

(defn claimed?
  [[x y] {:keys [left top width height]}]
  (if (and (inside-1d? x left width)
           (inside-1d? y top  height))
    true
    false))

(defn overlap?
  "Returns true if square inch overlaps given coordinates and
  a sequence of claims."
  [all-claims coordinates]
  (loop [claims    all-claims
         occupied? false]
    (if (empty? claims)
      false
      (if (claimed? coordinates (first claims))
        (if occupied?
          true
          (recur (rest claims)
                 true))
        (recur (rest claims)
               occupied?)))))
(
(defn read-claims
  [filename]
  (-> (slurp filename)
      (str/split #"\n")
      (->> (map parse-claim))))

(defn part1 []
  (let [claims (read-claims "./resources/day03/input.txt")]
    (->> (for [x (range 100) y (range 100)] [x y])
         (pmap #(overlap? claims %))
         (filter true?)
         (count))))

(time (part1)) ; =>112378
;;; "Elapsed time: 222476.748378 msecs" :/


;;; part 2
(defn directional-corner-overlap?
  [a b]
  (let [top-left  [(:left a) (:top a)]
        top-right [(+ (:left a) (dec (:width a))) (:top a)]
        bot-left  [(:left a)   (+ (:top a) (dec (:height a)))]
        bot-right  [ (+ (:left a) (dec (:width a)))   (+ (:top a) (dec (:height a)))]]
    (if (or (claimed? top-left  b)
            (claimed? top-right b)
            (claimed? bot-left  b)
            (claimed? bot-right b))
      true
      false)))


(defn directional-intersect-overlap?
  [a b]
  (let [top-a (:top a)
        bot-a (+ (:top a) (dec (:height a)))
        left-b (:left b)
        right-b (+ (:left b) (dec (:width b)))]
    (if (or (and (claimed? [left-b  top-a] b)
                 (claimed? [left-b  top-a] a))
            (and (claimed? [right-b top-a] b)
                 (claimed? [right-b top-a] a))
            (and (claimed? [left-b  bot-a] b)
                 (claimed? [left-b  bot-a] a))
            (and (claimed? [right-b bot-a] b)
                 (claimed? [right-b bot-a] a)))
      true
      false)))


(defn pair-overlap?
  [a b]
  (if (or (directional-corner-overlap? a b)
          (directional-corner-overlap? b a)
          (directional-intersect-overlap? a b)
          (directional-intersect-overlap? b a))
    true
    false))
    

(defn has-overlap?
  [claims a]
  (loop [c claims]
    (if (empty? c)
      false
      (if (= (first c) a)
        (recur (rest c))
        (if (pair-overlap? a (first c))
          true
          (recur (rest c)))))))

(defn print-claim
  [c]
  (println (str (:id c) " @ " (:left c) "," (:top c) ": " (:width c) "x" (:height c))))

(defn part2 []
  (let [claims (read-claims "./resources/day03/input.txt")]
    (->> claims
         (remove #(has-overlap? claims %))
         (map print-claim)
         )))

(time (part2)) ;=>#603

