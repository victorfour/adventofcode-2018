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

;(matrix-view-lookup-benchmark 32 100)
;;"Elapsed time: 145.14599 msecs"
;(matrix-view-lookup-benchmark 32 300)
;;"Elapsed time: 1113.584885 msecs"
;(matrix-view-lookup-benchmark 32 1000)
;;"Elapsed time: 7213.510214 msecs"
;(matrix-view-lookup-benchmark 32 3000)
;;"Elapsed time: 60589.148434 msecs"


(defn matrix-lookup-benchmark
  [size iterations]
  (let [choices (for [x (repeatedly iterations #(rand-int size))
                      y (repeatedly iterations #(rand-int size))]
                  [x y])
        matrix (mrand/sample-rand-int [size size] 100)]
    (time (println (count (map (partial apply (partial m/select matrix)) choices))))))


;(matrix-lookup-benchmark 32 100)
;;"Elapsed time: 73.496681 msecs"
;(matrix-lookup-benchmark 32 300)
;;"Elapsed time: 597.438547 msecs"
;(matrix-lookup-benchmark 32 1000)
;;"Elapsed time: 6706.325098 msecs"
;(matrix-lookup-benchmark 32 3000)
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

;(set-lookup-benchmark 32 633 100)
;;"Elapsed time: 18.753075 msecs"
;(set-lookup-benchmark 32 633 300)
;;"Elapsed time: 144.048917 msecs"
;(set-lookup-benchmark 32 633 1000)
;;"Elapsed time: 1610.297279 msecs"
;(set-lookup-benchmark 32 633 3000)
;;"Elapsed time: 14895.495903 msecs"


(defn get-in-lookup-benchmark
  [size iterations]
  (let [choices (for [x (repeatedly iterations #(rand-int size))
                      y (repeatedly iterations #(rand-int size))]
                  [x y])
        matrix (mrand/sample-rand-int [size size] 100)]
    (time (println (count (map (partial get-in matrix) choices))))))
  

;(get-in-lookup-benchmark 32 100)
;;"Elapsed time: 18.192407 msecs"
;(get-in-lookup-benchmark 32 300)
;;"Elapsed time: 115.556845 msecs"
;(get-in-lookup-benchmark 32 1000)
;;"Elapsed time: 1266.728193 msecs"
;(get-in-lookup-benchmark 32 3000)
;;"Elapsed time: 11488.919592 msecs"


(defn nth-lookup-benchmark
  [size iterations]
  (let [choices (for [x (repeatedly iterations #(rand-int size))
                      y (repeatedly iterations #(rand-int size))]
                  [x y])
        matrix (range (* size size))]
    (time (println (count (map #(nth matrix (apply * %)) choices))))))


;(nth-lookup-benchmark 32 100)
;;"Elapsed time: 126.334824 msecs"
;(nth-lookup-benchmark 32 300)
;;"Elapsed time: 832.037978 msecs"
;(nth-lookup-benchmark 32 1000)
;;"Elapsed time: 5635.350586 msecs"
;(nth-lookup-benchmark 32 3000)
;;""Elapsed time: 47801.094126 msecs"


(defn parse-data
  [filename]
  (-> (slurp filename)
      (str/split #"\n")
      (->> (mapv vec))))


(defn parse-world
  [filename]
  (let [translate {\. :empty
                   \# :wall
                   \G [:goblin 200]
                   \E [:elf 200]}]
    (-> (slurp filename)
        (str/split #"\n")
        (->> (mapv #(mapv translate %))))))


;(println (parse-data "./resources/day15/input.txt"))
;(println (parse-world "./resources/day15/input.txt"))


(def world
  [[:wall :wall :wall :wall :wall]
   [:wall :empty [:elf 200] [:goblin 199] :wall]
   [:wall :empty [:elf 194] :empty :wall]
   [:wall :empty :empty :empty :wall]
   [:wall [:goblin 200] :empty :empty :wall]
   [:wall :wall :wall :wall :wall]])


(def world2
  [[:wall :wall :wall :wall :wall]
   [:wall :empty :empty :empty :wall]
   [:wall :empty :empty :empty :wall]
   [:wall :empty :empty :empty :wall]
   [:wall [:goblin 200] :empty :empty :wall]
   [:wall :wall :wall :wall :wall]])
   

(defn get-creatures
  [world]
  (->> world
       (mapcat identity)
       (interleave (for [i (range (count world))
                         j (range (count (first world)))]
                     [i j]))
       (partition 2)
       (filter (comp sequential? second))))

;(get-creatures world)


(defn find-target
  "Searches around recursively for target squares.
  When it finds closest targets, it selects the first one in reading order.
  When there are multiple paths to this target, it selects
  the square that's first in reading order as first step."
  [creature world])


(defn move-creature
  [creatures world target]
  (let [[initial-position creature] (first creatures)]
    [(conj (rest creatures) '(target creature))
     (-> world
         (assoc-in initial-position :empty)
         (assoc-in target creature))]))

(defn print-world
  [world]
  (let [dict {:elf \E
              :goblin \G
              :wall \#
              :empty \.}]
    (letfn [(translate [slot]
              (if (sequential? slot)
                (get dict (first slot))
                (get dict slot)))]
      (println (->> (map #(map translate %) world)
           (map #(str/join %))
           (str/join "\n"))))))

;(def world (parse-world "./resources/day15/input.txt"))
;(def creatures (get-creatures world))
;(print-world world)
;(print-world (second (move-creature creatures world [2 20])))


(defn goto-target
  "Moves the first creature to the square that's one step closer to the closest target
  Returns a modified `creatures` and `world` with the updated position of
  the first creature, or the original ones."
  [creatures world]
  (if-let [target (find-target (first creatures) world)]
    (move-creature creatures world target)
    [creatures world]))
  

(defn surrounding-pos
  [position]
  (let [surroundings [[-1 0] [0 -1] [0 1] [1 0]]]
    (mapv (partial m/add position) surroundings)))

;(surrounding-pos [1 1])


(defn update-creatures
  [creatures updated-creature]
  (->> (map (fn [creature]
              (if (= (first creature) (first updated-creature))
                updated-creature
                creature))
            creatures)
       (filter #(> (second (second %)) 0))))

(defn update-world
  [world updated-creature]
  (if (> (second (second updated-creature)) 0)
    (assoc-in world (first updated-creature) (second updated-creature))
    (assoc-in world (first updated-creature) :empty)))
       
  
(defn hit
  "Get the content of squares surrounding the first creature in reading order.
  If one enemy is found, hit it by returning altered versions of `creatures` and `world`.
  Otherwise, return original `creatures`and `world`"
  [creatures world]
  (let [self-type (first (second (first creatures)))
        surroundings (surrounding-pos (first (first creatures)))
        enemies (->> (map #(get-in world %) surroundings)
                     (interleave surroundings)
                     (partition 2)
                     (filter (comp sequential? second))
                     (filter #(not= self-type (first (second %)))))]
    (when (seq enemies)
      (let [enemy-position (first (first enemies))
            enemy-type (first (second enemies))
            enemy-hp (- (second (second enemies)) 3)
            updated-enemy [enemy-position [enemy-type enemy-hp]]]
        [(update-creatures creatures updated-enemy) (update-world world updated-enemy)]))))

;(def creatures (get-creatures world))
;(first creatures)
;(not (empty? (hit creatures world)))
;(println (hit! creatures world))


(defn step
  [world]
  (let [creatures (get-creatures world)]
    (loop [c creatures
           w world]
      (if (seq c)
        (let [[current-c current-w] (goto-target c w)]
          (if-let [[new-c new-w] (hit current-c current-w)]
            (recur (rest new-c)
                   new-w)
            (recur (rest current-c)
                   current-w)))
        w))))

;;no trapped creature in intial map
;;=> game ends when there is only one type of creature left
(defn game-on?
  [world]
  (->> world
       (mapcat identity)
       (filter sequential?)
       (group-by first)
       (count)
       (< 1)))

(game-on? world)
(game-on? world2)
  

(defn score
  [world n-rounds]
  (* n-rounds
     (->> world
          (mapcat identity)
          (filter sequential?)
          (map second)
          (apply +))))

;(score world 3)
     

(defn part1
  [filename]
  (let [world (parse-world filename)
        rounds (take-while game-on? (iterate step world))]
    (score (last rounds) (dec (count rounds)))))

(sorted-map [1 2] 200 [0 1] 200 [3 4] 100 [0 0] 400)

(apply sorted-map (mapcat identity {[1 2] 200 [0 1] 200 [3 4] 100 [0 0] 400}))


         


