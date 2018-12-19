(ns adventofcode-2018.day09
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))


;;; part 1


(defn rotate
  [^java.util.ArrayDeque d n]
  (let [clockwise #(.addLast d (.removeFirst d))
        counter-clockwise #(.addFirst d (.removeLast d))]
    (if (> n 0)
      (repeatedly n clockwise)
      (repeatedly (- n) counter-clockwise))))


(defn init-state
  [num-players]
  {:num-marbles 1
   :num-players num-players
   :circle (java.util.ArrayDeque. [0])
   :current-player 0
   :current-marble-val 1
   :scores (vec (repeat num-players 0))})


(defn place-marble
  [{:keys [circle current-player num-players current-marble-val] :as state}]
  (doall (rotate circle 2))
  (doall (.addFirst circle current-marble-val))
    (-> state
        (update :current-marble-val inc)
        (update :current-player #(mod (inc %) num-players))))


(defn remove-marble
  [{:keys [scores circle num-players current-player current-marble-val] :as state}]
  (doall (rotate circle -7))
  (let [removed-marble (.removeFirst circle)
        points  (+ (nth scores current-player) removed-marble current-marble-val)
        new-current-player (mod (inc current-player) num-players)
        new-scores (assoc scores current-player points)]
    (-> state
        (update :current-marble-val inc)
        (update :current-player #(mod (inc %) num-players))
        (assoc :scores new-scores))))


(defn game-step
  [{:keys [current-marble-val] :as state}]
  (if (zero? (mod current-marble-val 23))
    (remove-marble state)
    (place-marble state)))


(defn part1
  [num-players last-marble]
  (->> (init-state num-players)
       (iterate game-step)
       (take (inc last-marble))
       (last)
       (:scores)
       (apply max)))


(time (println (part1 9 25)))
(time (println (part1 10 1618)))
(time (println (part1 13 7999)))
(time (println (part1 17 1104)))
(time (println (part1 21 6111)))
(time (println (part1 30 5807)))
(time (println (part1 452 71250)))
;;;=>388844
;;;"Elapsed time: 402.500885 msecs"
;;;old version: "Elapsed time: 383323.694446 msecs"


;;; part 2

(defn part2 []
  (part1 452 7125000))

(time (println (part2)))
;;;=>3212081616
;;;"Elapsed time: 45843.085438 msecs"

