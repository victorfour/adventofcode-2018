(ns adventofcode-2018.day11
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.core.matrix :as m]))


;;; part 1

;;x,y notation,
;; top-left cell  : (  1, 1)
;; top-right cell : (300, 1)

;; for computation, using  top-left  (  0, 0)
;;                         top-right (299, 0)


(defn hundreds
  [n]
  (-> (nth (reverse (str n)) 2 \0)
      (int)
      (- (int \0))))


(defn power-level
  [serial-number coords]
  (let [rack-id (+ (first coords) 10)]
    (-> rack-id
        (* (second coords))
        (+ serial-number)
        (* rack-id)
        (hundreds)
        (- 5))))


(power-level 8 [3 5])
(power-level 57 [122 79])
(power-level 39 [217 196])
(power-level 71 [101 153])


(defn compute-grid
  [width height serial-number]
  (let [cell-power (partial power-level serial-number)]
    (->> (for [y (range height)
               x (range width)];; first coords (x) is iterated first when scanning lines
           (cell-power [x y]))
         (partition width))))


(defn conv-3x3
  [grid [x y]]
  (let [get-line #(nth grid %)
        line-conv (fn [line]
                    (reduce + (take 3 (drop x line))))]
    (->> (range y (+ y 3))
         (map get-line)
         (map line-conv)
         (reduce +))))


(defn conv-grid
  [grid]
  (let [width (- (count (first grid)) 2)
        height (- (count grid) 2)
        conv-filter (partial conv-3x3 grid)]
    (for [y (range height)
          x (range width)]
      (conv-filter [x y]))))


(defn argmax
  [input]
  (->> input
       (interleave (range (count input)))
       (partition 2)
       (apply max-key second)
       (first)))


(defn part1
  [serial-number width height]
  (let [conv-width (- width 2)
        conv-height (- height 2)
        conved-grid (conv-grid (compute-grid width height serial-number))
        index-max-conved (argmax conved-grid)
        x-max (mod index-max-conved conv-width)
        y-max (/ (- index-max-conved x-max) conv-width)]
    [x-max y-max]))
    

(println (part1 18 300 300))
(println (part1 42 300 300))
(time (println (part1 8868 300 300)))
;;; [241 40]
;;;"Elapsed time: 4536.392848 msecs"


;;; part2

(defn conv-filter-size
  [grid filter-size [x y]]
  (let [line-conv (fn [y-line]
                    (reduce + (take filter-size (drop x (nth grid y-line)))))]
    (->> (range y (+ y filter-size))
         (map line-conv)
         (reduce +))))


(defn conv-grid-filter-size
  [grid filter-size]
  (let [width (- (count (first grid)) (dec filter-size))
        height (- (count grid) (dec filter-size))
        conv-filter (partial conv-filter-size grid filter-size)]
    (for [y (range height)
          x (range width)]
      (conv-filter [x y]))))


(defn conv-filter
  [serial-number filter-size]
  (let [width 300
        height 300
        conv-width (- width (dec filter-size))
        conv-height (- height (dec filter-size))
        conved-grid (conv-grid-filter-size (compute-grid width height serial-number) filter-size)
        index-max-conved (argmax conved-grid)
        max-value (apply max conved-grid)
        x-max (mod index-max-conved conv-width)
        y-max (/ (- index-max-conved x-max) conv-width)]
    [filter-size max-value x-max y-max]))


(time (println (conv-filter 8868 3)))

(defn part2
  [serial-number]
  (let [apply-conv (partial conv-filter serial-number)]
    (->> (for [filter-size (range 1 301)]
           filter-size)
         (pmap apply-conv)
         (apply max-key second))))


(time (println (part2 8868)))

;;;=>[12 71 166 75]
;;;answer: 166,75,12
;;;"Elapsed time: 6567538.537361 msecs" :/
