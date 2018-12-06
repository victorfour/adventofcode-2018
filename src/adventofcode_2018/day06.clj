(ns adventofcode-2018.day06
  (:require [clojure.string :as str]))

                

(defn add-dict ;;yep...
  "Adds a new point-coord to m.
  m is a map that has shape:
  {x {y1 id1
      y2 id2}}"
  [m point-coord]
  (let [id   (first point-coord)
        elem (second point-coord)
        x    (first elem)
        y    (second elem)] ;coords are transposed wrt conventions
    (if-let [x-map (get m x)]
      (assoc m x (assoc x-map y id))
      (assoc m x {y id}))))

(def filename "./resources/day06/example.txt")

(->> (slurp filename)
     (str/split-lines)
     (map (fn [line]
            (->> (rest (re-find #"(\d+), (\d+)" line))
                 (map #(Long/parseLong %)))))
     (interleave (range))
     (partition 2)
     ;(reduce add-dict {})
    )

(defn parse-data
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map (fn [line]
              (->> (rest (re-find #"(\d+), (\d+)" line))
                   (map #(Long/parseLong %)))))
       (interleave (range))
       (partition 2)
       (reduce add-dict {})))

(def parsed-data (memoize parse-data))

(parsed-data  "./resources/day06/example.txt")

(defn -max-x
  "Returms max x value"
  [data]
  (apply max (keys data)))

(defn -max-y
  "Returns max y value"
  [data]
  (->> (vals data)
     (map #(into [] %))
     (mapcat identity)
     (map first)
     (apply max)))

(def max-x (memoize -max-x))
(def max-y (memoize -max-y))

(max-x (parsed-data  "./resources/day06/example.txt"))
(max-y (parsed-data  "./resources/day06/example.txt"))


(defn get-point [x y data]
  (let [x-max (max-x data)
        y-max (max-y data)]
    (if (or (< x 0) (< y 0) (> x x-max) (> y y-max))
      :out-of-bound
      (when-let [x-map (get data x)]
        (get x-map y nil)))))

(get-point 1 1 (parsed-data "./resources/day06/example.txt"))

(apply distinct? [nil nil 1])

(defn duplicates?
  [coll]
  (let [no-nil (remove nil? coll)]
    (if (or (some #(< % 0) no-nil)
            (not (apply distinct? no-nil)))
      true
      false)))

(first (remove  nil? [1 2 3]))

;(duplicates? [nil nil nil 1 1 1 1 ])
;(duplicates? [nil nil nil 1 -1 ])
;(duplicates? [nil nil nil 1 3 ])

(defn -closest-point
   [x y data]
   (if-let [id (get-point x y data)]
     id
     (let [next-step [(-closest-point (- x 1) y data)
                      (-closest-point (+ x 1) y data)
                      (-closest-point x (- y 1) data)
                      (-closest-point x (+ y 1) data)]]
       (if (duplicates? next-step)
         -1
         (when-let [ret (first (remove nil? next-step))]
           ret)))))

(def closest-point (memoize -closest-point))
     
(closest-point 0 0 (parsed-data "./resources/day06/example.txt"))
;;;=> stackoveflow... can't figure out how to memoize this..
;;; let's do it the other way


(defn parse-data
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map (fn [line]
              (->> (rest (re-find #"(\d+), (\d+)" line))
                   (map #(Long/parseLong %)))))
       (interleave (range))
       (partition 2)))


(def parsed-data (parse-data filename))

(defn distance
  [[xa ya] [id [xb yb]]]
  [id (+ (StrictMath/abs (- xa xb)) (StrictMath/abs (- ya yb))) ])


(defn boundaries
  [data]
  (let [raw (map second parsed-data)]
    (->> raw
         (apply mapv vector)
         (map #(apply max %)))))


(defn closest-point
  [[x y]]
  (->> parsed-data
       (map #(distance [x y] %))
       (apply min-key second)
       (first)))


(frequencies '(1 2 3 4 4 4))
         
(let [[x-max y-max] (boundaries parsed-data)]
  (for [x (range x-max)
        y (range y-max)]
    (->> [x y]
         (closest-point)
         ;(frequencies)
         )))



                    
    


     
