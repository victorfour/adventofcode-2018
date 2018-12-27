(ns adventofcode-2018.day14
  (:require [clojure.string :as str]))



(defn init-state
  []
  {:recipes [3 7 1 0 1 0]
   :felf 4
   :self 3})


(defn concat-recipes
  [recipes n]
  (if (< 9 n)
    (conj (conj recipes (int (/ n 10))) (int (mod n 10)))
    (conj recipes n)))
   

(defn make
  [{:keys [recipes felf self] :as state}]
  (let [frecipe (get recipes felf)
        srecipe (get recipes self)
        new-recipes (concat-recipes recipes (+ frecipe srecipe))
        new-len (count new-recipes)
        new-felf (mod (inc frecipe) new-len)
        new-self (mod (inc srecipe) new-len)]
    (-> state
        (assoc :recipes new-recipes)
        (assoc :felf new-felf)
        (assoc :self (if (= new-felf new-self)
                       (mod (inc new-felf) new-len)
                       new-self
                       )))))


(defn part1
  [start-seq end]
  (let [end-len (+ start-seq 10)]
    (loop [state (init-state)
           i 0]
      (println state)
      (when (< i end)
        (if (> (count (get state :recipes)) end-len)
          (subvec (:recipes state) start-seq end-len)
          (recur (make state)
                 (inc i)))))))

(time (part1 236021 10))
                 
