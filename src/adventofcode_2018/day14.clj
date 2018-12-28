(ns adventofcode-2018.day14
  (:require [clojure.string :as str]))


;; part 1
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
  (let [fscore (get recipes felf)
        sscore (get recipes self)
        new-recipes (concat-recipes recipes (+ fscore sscore))
        new-len (count new-recipes)]
    (-> state
        (assoc :recipes new-recipes)
        (assoc :felf (mod (+ felf (inc fscore)) new-len))
        (assoc :self (mod (+ self (inc sscore)) new-len)))))


(defn part1
  [start-seq]
  (let [end-len (+ start-seq 10)]
    (loop [state (init-state)]
      (if (> (count (get state :recipes)) end-len)
        (str/join (map str (subvec (:recipes state) start-seq end-len)))
        (recur (make state))))))

(time (println (part1 236021)))
;;=>6297310862
;;=>"Elapsed time: 482.717829 msecs"

;;when using a string to store recipes : "Elapsed time: 7710.605768 msecs"

