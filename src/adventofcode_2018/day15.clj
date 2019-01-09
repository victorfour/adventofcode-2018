(ns adventofcode-2018.day15
  (:require [clojure.string :as str]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.random :as mrand]))


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


#_(def world1
  [[:wall :wall :wall :wall :wall]
   [:wall :empty [:elf 200] [:goblin 199] :wall]
   [:wall :empty [:elf 194] :empty :wall]
   [:wall :empty :empty :empty :wall]
   [:wall [:goblin 200] :empty :empty :wall]
   [:wall :wall :wall :wall :wall]])


#_(def world2
  [[:wall :wall :wall :wall :wall]
   [:wall :empty :empty :empty :wall]
   [:wall :empty :empty :empty :wall]
   [:wall :empty :empty :empty :wall]
   [:wall [:goblin 200] :empty :empty :wall]
   [:wall :wall :wall :wall :wall]])

#_(def world3
  [[:wall :wall :wall :wall :wall]
   [:wall [:elf 200] :empty :empty :wall]
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

;(first (get-creatures world))
;(apply hash-map (first (get-creatures world)))


(defn enemy-types?
  [creature-type-one creature-type-two]
  (or (and (= creature-type-one :elf) (= creature-type-two :goblin))
      (and (= creature-type-two :elf) (= creature-type-one :goblin))))
    

(defn surrounding-pos
  [position]
  (let [surroundings [[-1 0] [0 -1] [0 1] [1 0]]]
    (map (partial m/add position) surroundings)))


;(surrounding-pos [1 1])

(defn surrounding-enemy?
  [creature-pos world]
  (let [creature-type (first (get-in world creature-pos))]
    ;(println (str "creature type: " creature-type))
    (some->> (surrounding-pos creature-pos)
             (map (partial get-in world))
             (filter sequential?)
             (map first)
             (some (partial enemy-types? creature-type)))))

;(surrounding-enemy? [1 2] (parse-world "./resources/day15/example1.txt"))
;(surrounding-enemy? [2 4] (parse-world "./resources/day15/example1.txt"))


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


(merge (sorted-map [1 2] :three [0 1] :two [4 0] :four)  (hash-map [5 2] :five [0 0] :one))
(merge (hash-map [1 2] :three [0 1] :two [4 0] :four)  (sorted-map [5 2] :five [0 0] :one))

(defn step-front
  ;"Extends the front one step further in reading order.
  ;Propagates first-step direction.
  ;Needs to keep an actualized set of seen
  ;Returns new front (as sorted-map) and new seen (set)"
  [seen front world creature-type]
  (loop [f front
         new-front (sorted-map)
         s seen]
    (if (seq f)
      (let [first-step (second (first f))
            to-explore (-> (first f)
                           first
                           surrounding-pos
                           set
                           (clojure.set/difference s)
                           seq)
            new-seen (clojure.set/union s (set to-explore))
            explored (some->> to-explore
                              (remove #(= :wall (get-in world %)))
                              (map (fn [pos] [pos (get-in world pos)]))
                              (map (fn [[position mapval]] (if (sequential? mapval)
                                                             (when (enemy-types? (first mapval) creature-type)
                                                               [position [:enemy first-step]])
                                                             [position first-step])))
                              (mapcat identity)
                              (apply hash-map))]
        ;(println (str "creature-type: " creature-type))
        ;(println (str "first-step: " first-step))
        ;(println (str "seen:" s))
        ;(println (str "new-seen:" new-seen))
        ;(println (str "to-explore:" to-explore))
        ;(println (str "explored:" explored))
        (recur (rest f)
               (merge new-front explored)
               new-seen)

        ;(println (str "explored: " (seq explored))))
        )

      [new-front s])
    ))
    


(defn find-target
  "Searches around recursively for target squares
  When it finds closest targets, it selects the first one in reading order.
  When there are multiple paths to this target, it selects
  the square that's first in reading order as first step."
  [creature-pos world]
  (let [first-step (surrounding-pos creature-pos)]
    (loop [seen (conj (set first-step) creature-pos)
           front (-> first-step
                     (interleave first-step)
                     (->> (partition 2)
                          (remove #(= :wall (get-in world (first %))))
                          (mapcat identity)
                          (apply sorted-map)))]
      ;(println front)
      (let [[new-front new-seen] (step-front seen front world (first (get-in world creature-pos)))]
        (when (seq new-front)
          (if-let [toward-enemy (->> new-front
                                     (filter #(= (first (second %)) :enemy))
                                     first ;[[pos] [:enemy [first-step-pos]]]
                                     second
                                     second)]; sorry about that.. 
            toward-enemy
            (recur new-seen new-front)))))))


(defn move-creature
  [world creature-pos target]
  (let [creature (get-in world creature-pos)]
    (-> world
        (assoc-in creature-pos :empty)
        (assoc-in target creature))))


(defn goto-target
  "Moves the first creature to the square that's one step closer to the closest target
  Returns a modified `creatures` and `world` with the updated position of
  the first creature, or the original ones."
  [creature-pos world]
  (if-let [target (find-target creature-pos world)]
    (move-creature world creature-pos target)
    world))

(print-world (parse-world "./Resources/Day15/example2.txt"))

(print-world (goto-target [1 2] (parse-world "./resources/day15/example2.txt")))


(defn hit
  [creature-pos world]
  (let [self-type (first (get-in world creature-pos))
        surroundings (surrounding-pos creature-pos)
        enemies (->> (map (partial get-in world) surroundings)
                     (interleave surroundings)
                     (partition 2)
                     (filter (comp sequential? second))
                     (filter #(not= self-type (first (second %)))))]
    (if (seq enemies)
      (let [enemy (first enemies)
            enemy-position (first enemy)
            enemy-type (first (second enemy))
            enemy-hp (- (second (second enemy)) 3)]
        (if (<= enemy-hp 0)
          (assoc-in world enemy-position :empty)
          (assoc-in world enemy-position [enemy-type enemy-hp])))
      world)))

;(print-world (parse-world "./resources/day15/example1.txt"))

;(sort (map first (get-creatures (parse-world "./resources/day15/example1.txt"))))

;(= (hit [1 2] (parse-world "./resources/day15/example1.txt"))
;   (parse-world "./resources/day15/example1.txt"))

;(print-world (hit [2 4] (parse-world "./resources/day15/example1.txt")))
;(hit [2 4] (parse-world "./resources/day15/example1.txt"))


(defn step
  [world]
  (let [creatures (sort (map first (get-creatures world)))]
    (loop [c creatures
           w world]
      (if (seq c)
        (recur (rest c)
               (hit (first c) (if (surrounding-enemy? (first c) w)
                                w
                                (goto-target (first c) w))))
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


(defn score
  [world n-rounds]
  (* n-rounds
     (->> world
          (mapcat identity)
          (filter sequential?)
          (map second)
          (apply +))))

     

(defn part1
  [filename]
  (let [world (parse-world filename)
        rounds (take-while game-on? (iterate step world))]
    (score (last rounds) (dec (count rounds)))))

(println "-------------------------------------------")
(step (parse-world "./resources/day15/example1.txt"))

;(part1 "./resources/day15/input.txt")

