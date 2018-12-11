(ns adventofcode-2018.day09
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))


;;; part 1

(defn init-state
  [num-players last-marble]
  {:num-rounds (inc last-marble)
   :num-players num-players
   :circle [0]
   :current-marble-pos 0
   :current-marble-val 0
   :num-marbles 1
   :current-player 0
   :scores (repeat num-players 0)})

(init-state 9 25)


;(defn update-score
  ;[{:keys [scores current-player] :as state}]
  ;(assoc scores player (+ (nth scores player) player-points)))


(defn next-pos
  [{:keys [current-marble-pos circle] :as state}]
  (let [pos (mod (+ current-marble-pos 2) (count circle))]
    (if (zero? pos)
      (count circle)
      pos)))


(next-pos 1 2)
(next-pos 1 3)
(next-pos 3 4)
(next-pos 1 5)
(next-pos 3 6)
(next-pos 5 7)


(defn place-marble
  [{:keys [scores circle current-player current-marble-val num-players] :as state}]
  (let [new-marble-val (inc current-marble-val)
        pos (next-pos state)
        [head tail] (split-at pos circle)
        new-tail (conj tail new-marble-val)
        new-circle (concat head new-tail)
        new-current-player (mod (inc current-player) num-players)]
    (-> state
        (assoc :circle new-circle)
        (assoc :current-marble-val new-marble-val)
        (assoc :current-marble-pos pos)
        (assoc :current-player new-current-player)
        )))


;(nth [1 2 3] 1);=> 2

;(mod (- 2 3) 5)

;(assoc [0 1 2 3] 1 9)

(defn remove-marble
  [{:keys [scores circle num-players current-player current-marble-pos current-marble-val] :as state}]
  (let [new-marble-val (inc current-marble-val)
        to-remove-pos (mod (- current-marble-pos 7) (count circle))
        new-current-marble-pos (mod (inc to-remove-pos) (dec (count circle)))
        points  (+ (nth scores current-player) (nth circle to-remove-pos) new-marble-val)
        [head tail] (split-at to-remove-pos circle)
        new-circle (concat head (rest tail))
        new-current-player (mod (inc current-player) num-players)
        new-scores (assoc scores current-player points)
        ]
    (-> state
        (assoc :circle new-circle)
        (assoc :current-marble-val new-marble-val)
        (assoc :current-marble-pos new-current-marble-pos)
        (assoc :current-player new-current-player)
        (assoc :scores new-scores))))

  

(pp/pprint (init-state 9 25))
(remove-marble (last (take 23 (iterate place-marble (init-state 9 25)))))
(count (:circle (last (take 23 (iterate place-marble (init-state 9 25))))))
(split-at 22 (:circle (last (take 23 (iterate place-marble (init-state 9 25))))))

(pp/pprint (:circle (place-marble (place-marble (init-state 9 25)))))
(pp/pprint (place-marble (place-marble (place-marble (init-state 9 25)))))

    


;(defn game-step
;  [{:keys [scores current-player current-marble num-marble]} :as state]
  
  
