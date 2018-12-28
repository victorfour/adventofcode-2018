(ns adventofcode-2018.day14
  (:require [clojure.string :as str]))


;; part 1
(defn init-state
  []
  {:recipes [3 7 1 0 1 0 1 2]
   :felf 0
   :self 6})


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


;; part2

(defn part2
  [input]
  (let [pattern (mapv #(Long/parseLong (str %)) (seq input))
        pattern-len (count pattern)]
    (loop [state (init-state)]
      (let [start-scan (- (count (:recipes state)) pattern-len)
            candidate1 (subvec (:recipes state) start-scan (+ start-scan pattern-len))
            candidate2 (subvec (:recipes state) (dec start-scan) (+ (dec start-scan) pattern-len))]
        (cond
          (= pattern candidate1) start-scan
          (= pattern candidate2) (dec start-scan)
          :else (recur (make state)))))))
      
(time (println (part2 "236021")))
;;=>20221334
;;"Elapsed time: 45571.993285 msecs"
