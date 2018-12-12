(ns adventofcode-2018.day10
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.core.matrix :as m]))
            
;;; part 1


(def exampletxt "./resources/day10/example.txt")
(def inputtxt "./resources/day10/input.txt")

(defn parse-line
  [line]
  (let [
        pattern #"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+).+"
        found   (rest (re-find pattern line))
        ]
    (map #(Long/parseLong %) found)
        ))


(defn parse-data
  [input]
  (let [[positions velocities ] (-> (slurp input)
                                    (str/trim)
                                    (str/split-lines)
                                    (->> (map parse-line)
                                         (m/transpose)
                                         (partition 2)))]
    [positions velocities]))


(defn next-second
  [velocities positions]
  (m/add positions velocities))

(def on \#)
(def off \.)

(defn fill-grid
  [positions]
  (let [
        y-max (apply max (second positions))
        y-min (apply min (second positions))
        x-max (apply max (first positions))
        x-min (apply min (first positions))
        x-offset (Math/abs x-min)
        y-offset (Math/abs y-min)
        x-range (inc (- x-max x-min))
        y-range (inc (- y-max y-min))
        offset-positions [(m/add x-offset (first positions)) (m/add y-offset (second positions))]
        pos (m/transpose offset-positions)
        grid (m/mutable (partition x-range (vec (repeat (* x-range y-range) off))))]
    (loop [points pos]
      (if (empty? points)
        grid
        (let [point (first points)]
          (m/mset! grid (second point) (first point) on)
          (recur (rest points)))))))


(defn get-range
  [positions]
  (let [
        y-max (apply max (second positions))
        y-min (apply min (second positions))
        x-max (apply max (first positions))
        x-min (apply min (first positions))
        x-range (inc (- x-max x-min))
        y-range (inc (- y-max y-min))]
    [x-range y-range]
    ))


(defn display
  [positions]
  (let [grid (fill-grid positions)
        [height width] (m/shape grid)]
    (dotimes [n height]
      (println (nth grid n)))))


(time (let [[positions velocities] (parse-data "./resources/day10/example.txt")
      step (partial next-second velocities)]
  (println (display (last (take 4 (iterate step positions)))))))

(defn part1
  [input]
  (let [[positions velocities] (parse-data input)
        step (partial next-second velocities)
        initial-range (get-range positions)]
    (loop [current-positions positions
           current-range initial-range]
      (let [next-positions (step current-positions)
            next-range (get-range next-positions)]
        (if (> (apply * next-range) (apply * current-range))
          (display current-positions)
          (recur next-positions
                 next-range))))))

(part1 "./resources/day10/input.txt")
      

  

