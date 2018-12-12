(ns adventofcode-2018.day09
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))


;;; part 1

(defn init-state
  [num-players]
  {:num-players num-players
   :circle [0]
   :current-marble-pos 0
   :current-marble-val 1 
   :current-player 0
   :scores (vec (repeat num-players 0))})


(defn next-pos
  [{:keys [current-marble-pos circle] :as state}]
  (let [pos (mod (+ current-marble-pos 2) (count circle))]
    (if (zero? pos)
      (count circle)
      pos)))


(defn place-marble
  [{:keys [scores circle current-player current-marble-val num-players] :as state}]
  (let [new-marble-val (inc current-marble-val)
        pos (next-pos state)
        [head tail] (split-at pos circle)
        new-tail (conj tail current-marble-val)
        new-circle (concat head new-tail)
        new-current-player (mod (inc current-player) num-players)]
    (-> state
        (assoc :circle new-circle)
        (assoc :current-marble-val new-marble-val)
        (assoc :current-marble-pos pos)
        (assoc :current-player new-current-player))))


(defn remove-marble
  [{:keys [scores circle num-players current-player current-marble-pos current-marble-val] :as state}]
  (let [new-marble-val (inc current-marble-val)
        to-remove-pos (mod (- current-marble-pos 7) (count circle))
        new-current-marble-pos (mod to-remove-pos (dec (count circle)))
        points  (+ (nth scores current-player) (nth circle to-remove-pos) current-marble-val)
        [head tail] (split-at to-remove-pos circle)
        new-circle (concat head (rest tail))
        new-current-player (mod (inc current-player) num-players)
        new-scores (assoc scores current-player points)]
    (-> state
        (assoc :circle new-circle)
        (assoc :current-marble-val new-marble-val)
        (assoc :current-marble-pos new-current-marble-pos)
        (assoc :current-player new-current-player)
        (assoc :scores new-scores))))


(defn game-step
  [{:keys [current-marble-val] :as state}]
  (if (and (> current-marble-val 0)
           (zero? (mod current-marble-val 23)))
    (remove-marble state)
    (place-marble state)))


(defn part1
  [num-players last-marble]
  (apply max (:scores (last (take (inc last-marble) (iterate game-step (init-state num-players)))))))
    

(time (part1 9 25))
(time (part1 10 1618))
(time (part1 13 7999))
(time (part1 17 1104))
(time (part1 21 6111))
(time (part1 30 5807))
(time (println (part1 452 71250)))
;;;=>388844
;;;"Elapsed time: 658258.725558 msecs"

;;; part2 is (part1 452 71250), but that's gonna take > 18 hours with this code...

  
  
