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


(defn enemies?
  [creature-one-type creature-two-type]
  (or (and (= creature-one-type :elf)(= creature-two-type :goblin))
      (and (= creature-two-type :elf)(= creature-one-type :goblin))))
    

(defn surrounding-pos
  [position]
  (let [surroundings [[-1 0] [0 -1] [0 1] [1 0]]]
    (mapv (partial m/add position) surroundings)))


;(surrounding-pos [1 1])

(defn surrounding-enemy?
  [creature world]
  (let [surroundings (surrounding-pos (first creature))]
    (->> (surrounding-pos (first creature))
         (map (partial get-in world))
         (some (partial enemies? creature)))))


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


(defn step-front
  "Extends the front one step further in reading order.
  Propagates first-step direction.
  Needs to keep an actualized set of seen
  Returns new front (as sorted-map) and new seen (set)"
  [seen front world creature-type]
  (println (str "front:" front))
  (println (str "seen:" seen))
  (loop [f front
         new-front (sorted-map)
         s seen]
    (if (seq f)
      (let [first-step (second (first f))
            to-explore (-> (first f)
                           first
                           surrounding-pos
                           set
                           (clojure.set/difference s))
            explored (some->> (interleave to-explore (map #(get-in world %) to-explore))
                              (partition 2)
                              (remove #(= (second %) :wall))
                              (map (fn [[position in-map]] (if (sequential? in-map)
                                                           (when (enemies? (first in-map) creature-type)
                                                             [position [first-step (first in-map)]])
                                                           [position first-step])))
                              (println)
                              #_(apply sorted-map)
                              )]
        (println (str "to-expl:" to-explore))
        (println (str "explored:" explored))
        (recur (rest f)
               new-front
               (clojure.set/union s to-explore)))
      [new-front seen])))


(defn remove-walls-from-front
  [world front]
  (println (str "blah: " front))
  (print-world world)
  (remove #(= :wall (get-in world (first %))) front))

  

(defn find-target
  "Searches around recursively for target squares
  When it finds closest targets, it selects the first one in reading order.
  When there are multiple paths to this target, it selects
  the square that's first in reading order as first step."
  [creature world]
  (loop [front (-> (surrounding-pos (first creature))
                   (interleave [:up :left :right :down])
                   (->> (partition 2)
                        (remove-walls-from-front world)
                        (mapcat identity)
                        (apply sorted-map)
                        ))

         seen (conj (set (keys front)) (first creature))]
    (let [[new-front new-seen] (step-front seen front world (first creature))]
      (when (not (empty? new-front))
        (if-let [toward-enemy (->> new-front
                                   (map second)
                                   (some sequential?) ;enemy in front := {[i j] [:first-step :enemy]}
                                   first)]
          (first toward-enemy)
          (recur new-front
                 new-seen))))))
  


(defn move-creature
  [creatures world target]
  (let [[initial-position creature] (first creatures)]
    [(conj (rest creatures) '(target creature))
     (-> world
         (assoc-in initial-position :empty)
         (assoc-in target creature))]))



(defn goto-target
  "Moves the first creature to the square that's one step closer to the closest target
  Returns a modified `creatures` and `world` with the updated position of
  the first creature, or the original ones."
  [creatures world]
  (if-let [target (find-target (first creatures) world)]
    (move-creature creatures world target)
    [creatures world]))


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


(defn step
  [world]
  (let [creatures (get-creatures world)]
    (loop [c creatures
           w world]
      (if (seq c)
        (let [[current-c current-w] (if (surrounding-enemy? (first c) w)
                                      [c w]
                                      (goto-target c w))]
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

(step (parse-world "./resources/day15/example1.txt"))

;(part1 "./resources/day15/input.txt")

