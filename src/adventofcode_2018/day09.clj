(ns adventofcode-2018.day09
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))



(defn init-state
  [num-players last-marble]
  {:num-rounds (inc last-marble)
   :num-players num-players
   :circle [0]
   :current-marble 1
   :num-marbles 1
   :current-player 0
   :scores (repeat num-players 0)})

(init-state 9 25)


;(defn update-score
  ;[{:keys [scores current-player] :as state}]
  ;(assoc scores player (+ (nth scores player) player-points)))


(defn next-pos
  [{:keys [current-marble num-marbles] :as state}]
  (let [pos (mod (+ current-marble 2) num-marbles)]
    (if (zero? pos)
      num-marbles
      pos)))

(next-pos 1 2)
(next-pos 1 3)
(next-pos 3 4)
(next-pos 1 5)
(next-pos 3 6)
(next-pos 5 7)


(defn place-marble
  [{:keys [scores circle current-player current-marble] :as state}]
  (let [pos (next-pos state)
        [head tail] (split-at pos circle)
        new-tail (conj tail current-marble)
        new-circle (concat head new-tail)]
    (assoc state :circle new-circle)))


(defn remove-marble
  [{:keys [scores circle current-player current-marble] :as state}]

  

(pp/pprint (init-state 9 25))
(pp/pprint (place-marble (init-state 9 25)))

    


;(defn game-step
;  [{:keys [scores current-player current-marble num-marble]} :as state]
  
  
