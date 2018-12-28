(ns adventofcode-2018.day15
  (:require [clojure.string :as str]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.random :as mrand]))


;;; what's the size of the problem?

(-> (slurp "./resources/day15/input.txt")
    (str/replace #"\n" "")
    (str/replace #"\." "")
    count) ;=>633

(-> (slurp "./resources/day15/input.txt")
    (str/split #"\n")
    count);=>32 lines

(-> (slurp "./resources/day15/input.txt")
    (str/split #"\n")
    first
    count);=>32 columns

;;;some benchmarks

(defn matrix-view-lookup-benchmark
  [size iterations]
  (let [choices (for [x (repeatedly iterations #(rand-int size))
                      y (repeatedly iterations #(rand-int size))]
                  [x y])
        matrix (mrand/sample-rand-int [size size] 100)]
    (time (println (count (map (partial apply (partial m/select-view matrix)) choices))))))

(matrix-view-lookup-benchmark 32 100)
;;"Elapsed time: 145.14599 msecs"
(matrix-view-lookup-benchmark 32 300)
;;"Elapsed time: 1113.584885 msecs"
(matrix-view-lookup-benchmark 32 1000)
;;"Elapsed time: 7213.510214 msecs"
(matrix-view-lookup-benchmark 32 3000)
;;"Elapsed time: 60589.148434 msecs"


(defn matrix-lookup-benchmark
  [size iterations]
  (let [choices (for [x (repeatedly iterations #(rand-int size))
                      y (repeatedly iterations #(rand-int size))]
                  [x y])
        matrix (mrand/sample-rand-int [size size] 100)]
    (time (println (count (map (partial apply (partial m/select matrix)) choices))))))


(matrix-lookup-benchmark 32 100)
;;"Elapsed time: 73.496681 msecs"
(matrix-lookup-benchmark 32 300)
;;"Elapsed time: 597.438547 msecs"
(matrix-lookup-benchmark 32 1000)
;;"Elapsed time: 6706.325098 msecs"
(matrix-lookup-benchmark 32 3000)
;;;"Elapsed time: 67559.543497 msecs"


(defn set-lookup-benchmark
  [size set-size iterations]
  (let [choices (for [x (repeatedly iterations #(rand-int size))
                      y (repeatedly iterations #(rand-int size))]
                  [x y])
        test-set (set (for [x (repeatedly set-size #(rand-int size))
                            y (repeatedly set-size #(rand-int size))]
                        [x y]))]
    (time (println (count (map test-set choices))))))

(set-lookup-benchmark 32 633 100)
;;"Elapsed time: 18.753075 msecs"
(set-lookup-benchmark 32 633 300)
;;"Elapsed time: 144.048917 msecs"
(set-lookup-benchmark 32 633 1000)
;;"Elapsed time: 1610.297279 msecs"
(set-lookup-benchmark 32 633 3000)
;;"Elapsed time: 14895.495903 msecs"


(defn get-in-lookup-benchmark
  [size iterations]
  (let [choices (for [x (repeatedly iterations #(rand-int size))
                      y (repeatedly iterations #(rand-int size))]
                  [x y])
        matrix (mrand/sample-rand-int [size size] 100)]
    (time (println (count (map (partial get-in matrix) choices))))))
  

(get-in-lookup-benchmark 32 100)
;;"Elapsed time: 18.192407 msecs"
(get-in-lookup-benchmark 32 300)
;;"Elapsed time: 115.556845 msecs"
(get-in-lookup-benchmark 32 1000)
;;"Elapsed time: 1266.728193 msecs"
(get-in-lookup-benchmark 32 3000)
;;"Elapsed time: 11488.919592 msecs"


(defn nth-lookup-benchmark
  [size iterations]
  (let [choices (for [x (repeatedly iterations #(rand-int size))
                      y (repeatedly iterations #(rand-int size))]
                  [x y])
        matrix (range (* size size))]
    (time (println (count (map #(nth matrix (apply * %)) choices))))))


(nth-lookup-benchmark 32 100)
;;"Elapsed time: 126.334824 msecs"
(nth-lookup-benchmark 32 300)
;;"Elapsed time: 832.037978 msecs"
(nth-lookup-benchmark 32 1000)
;;"Elapsed time: 5635.350586 msecs"
(nth-lookup-benchmark 32 3000)
;;""Elapsed time: 47801.094126 msecs"
