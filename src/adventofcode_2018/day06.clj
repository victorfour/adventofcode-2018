(ns adventofcode-2018.day06
  (:require [clojure.string :as str]))

                
(defn parse-data
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map (fn [line]
              (->> (rest (re-find #"(\d+), (\d+)" line))
                   (map #(Long/parseLong %)))))
       (interleave (range))
       (partition 2)))


(defn distance
  [[xa ya] [id [xb yb]]]
  [id (+ (StrictMath/abs (- xa xb)) (StrictMath/abs (- ya yb))) ])


(defn boundaries
  [parsed-data]
  (let [raw (map second parsed-data)]
    (->> raw
         (apply mapv vector)
         (map #(apply max %))
         (map inc))))


(defn -closest-point
  [data [x y]]
  (let [dists (->> data
                   (map #(distance [x y] %)))
        min-dist (apply min (map second dists))
        close-points (filter #(= (second %) min-dist) dists)]
    (if (> (count close-points) 1)
      -1
      (first (first close-points)))))


(defn part1
  [filename]
  (let [data (parse-data filename)
        [x-max y-max] (boundaries data)
        closest-point (partial -closest-point data)
        filled-map (->> (for [x (range x-max)
                              y (range y-max)]
                          [x y])
                        (pmap closest-point)
                        (partition y-max))
        top (first filled-map)
        bottom (last filled-map)
        left (map first filled-map)
        right (map last filled-map)
        infinite-elems (distinct (flatten [top bottom left right]))
        finite-map (map (fn [x] (remove #(some #{%} infinite-elems) x)) filled-map)]
    (println (apply max (vals (frequencies (flatten finite-map)))))))
    


;(time (part1 "./resources/day06/input.txt"))
;;; 3647
;;; "Elapsed time: 80004.132741 msecs"


;;; part2

(defn boundaries-2
  [raw]
  (->> raw
       (apply mapv vector)
       (map #(apply max %))
       (map inc)))


(defn parse-data-2
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map (fn [line]
              (->> (rest (re-find #"(\d+), (\d+)" line))
                   (map #(Long/parseLong %)))))))


(defn distance-2
  [[xa ya] [xb yb]]
  (+ (StrictMath/abs (- xa xb)) (StrictMath/abs (- ya yb))))


(defn sum-of-distances
  [data [x y]]
  (->> data
       (map #(distance-2 [x y] %))
       (apply +)))

(defn part2
  [filename threshold]
  (let [data (parse-data-2 filename)
        [x-max y-max] (boundaries-2 data)
        score (partial sum-of-distances data)]
    (->> (for [x (range x-max)
               y (range y-max)]
           [x y])
         (pmap score)
         (filter #(< % threshold))
         (count)
         (println))))
         

;(time (part2 "./resources/day06/input.txt" 10000))
;;; =>41605
;;;"Elapsed time: 74341.127299 msecs"


         

     
