(ns adventofcode-2018.day04
  (:require [clojure.string :as str]))


;;; part 1

(defn parse-line
  [line]
  (let [pattern #"\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] (Guard #\d+)?" 
        [y m d hour min guard] (rest (re-find pattern line))]
    {:year (Long/parseLong y)
     :month (Long/parseLong m)
     :day (Long/parseLong d)
     :hour (Long/parseLong hour)
     :minute (Long/parseLong min)
     :guard-change (when guard true)
     :guard (when guard
              (Long/parseLong (re-find #"\d+" guard))) }))


(defn fill-guard
  [left right]
  (if (nil? (:guard right))
    (assoc right :guard (:guard left))
    right))


(defn parse-data [input]
  (-> (slurp input)
      (str/split-lines)
      (->> (sort)
           (map parse-line)
           (reductions fill-guard))))

(def parsed-data (parse-data  "./resources/day04/input.txt" ))


(defn group-by-shift
  [parsed-data]
  (->> parsed-data
       (partition-by #(nil? (:guard-change %)))
       (partition 2)
       (map second)
       (flatten)
       (partition 2)))
  

(defn count-minutes-asleep-per-shift
  [shift]
  {:asleep (- (:minute (second shift)) (:minute (first shift)))
   :guard (:guard (first shift))})


(defn accumulate-sleep
  [summary]
  {:guard (first summary)
   :sleep (->> (second summary)
               (map :asleep)
               (reduce +))})


(defn get-sleepiest-guard []
  (->> parsed-data
       (group-by-shift)
       (map count-minutes-asleep-per-shift)
       (group-by :guard)
       (map accumulate-sleep)
       (reduce #(if (< (:sleep %1) (:sleep %2)) %2 %1))
       (:guard)))


(defn range-asleep
  [shift]
  (range (:minute (first shift)) (:minute (second shift))))
  

(defn get-sleepiest-minute
  [id]
  (->> parsed-data
       (group-by-shift)
       (filter #(= id (:guard (first %))))
       (map range-asleep)
       (flatten)
       (frequencies)
       (reduce #(if (< (second %1) (second %2)) %2 %1))
       (first)))


(defn part1 []
  (let [sleepiest-guard  (get-sleepiest-guard)
        sleepiest-minute (get-sleepiest-minute sleepiest-guard)]
    (* sleepiest-minute sleepiest-guard))) 
    
(part1) ;=>87681

  

;;; part 2


(defn range-asleep-per-guard
  [x]
  [(:guard (first x)) (range-asleep x)])


(defn minute-frequencies
  [x]
  [(first x)
   (->> (second x)
        (map second)
        (flatten)
        (frequencies))])
  
(defn max-freq
  [left right]
  (reduce #(if (< (second %left) (second %right)) %right %left)))
    
(->> parsed-data
     (group-by-shift)
     (take 30)
     (map range-asleep-per-guard)
     (group-by first)
     (map minute-frequencies)
     (max-freq)
     ;(reduce max-freq)
     ;(map second)
     ;(map interleave)
     ;(map get-range)
     (println)
     ;(map #(:guard first %))
     ;(map #({:guard (:guard (first %))}))
             ;:range (range-asleep %)}))
     )

