(ns adventofcode-2018.day07
  (:require [clojure.string :as str]))

;;; part 1

;; parsing:
;; - enumerate all the nodes
;; - create a map that goes from each node to []
;; - "invert" edge orientation
;; - for each relation, conj dependency
;; - parsing done

;; functions
;; - fn -> remove a given node from the graph
;; - fn -> walk as far as possible starting from all nodes,
;;   a dependency free node is linked to []
;;   returns an array

;; alg:
;; - if graph is empty, stop
;; - walk the graph to get array of distinct dependency free nodes
;; - sort it alphabetically
;; - extract first node from that list, and remove it from the graph
;; - recur


(defn parse-data
  "Returns a map representing a graph of dependencies.
  This map has shape {C [], A C}.
  In this example, C has no dependency. A depends on C"
  [filename]
  (let [ir (->> (slurp filename)
                (str/split-lines)
                (map #(re-find #"Step ([A-Z]) must be finished before step ([A-Z]).+" %))
                (map rest)
                (map #(vector (first (second %)) (first (first %)))))
        all-nodes (set (flatten ir))
        empty-graph  (apply hash-map (interleave (vec all-nodes) (repeat [])))]
    (reduce (fn [graph edge]
              (assoc graph
                     (first edge)
                     (conj (get graph (first edge)) (second edge))))
            empty-graph
            ir)))


(defn remove-node
  "Duh!"
  [graph node]
  (->> graph
       (map (fn [edge]
              [(first edge)
               (vec (remove #(= node %) (second edge)))]))
       (remove #(= node (first %)))
       (mapcat identity)
       (apply hash-map)))


(defn walk
  "From one node, walk the graph to find nodes that leads to [].
  Those nodes are dependency free nodes that can be reached starting
  from given `node` argument.
  Returns an array of characters representing those nodes"
  ([graph node] 
   (walk graph node []))
  ([graph node acc]
   (let [step (get graph node)]
     (if (empty? step)
       (vec (set (flatten (conj acc node))))
       (vec (set (flatten (concat (map #(walk graph % acc) step)))))))))


(defn walk-graph
  "From all nodes in the graph, uses walk fn to find dep free nodes.
  Returns an array of all dependency free nodes"
  [graph]
  (let [all-nodes (keys graph)]
    (vec (sort (vec (set (flatten (map #(walk graph %) all-nodes))))))))


(defn part1
  ([graph]
   (part1 graph []))
  ([graph acc]
   (if (empty? graph)
     (apply str acc)
     (let [dep-free-nodes (walk-graph graph)
           next-graph (remove-node graph (first dep-free-nodes))]
       (recur next-graph (conj acc (first dep-free-nodes)))))))

(time (println (part1 (parse-data "./resources/day07/example.txt"))))
;;;(time (println (part1 (parse-data "./resources/day07/input.txt"))))
;;;=>AEMNPOJWISZCDFUKBXQTHVLGRY
;;; "Elapsed time: 608.031696 msecs"


;;; part 2

(defn init-state
  [input-file offset num-workers]
  {:graph (parse-data input-file)
   :todo '()
   :workers '() 
   :elapsed-time 0
   :offset offset
   :num-workers num-workers})


(defn time-needed
  [task offset]
  (+ offset (inc (- (int task) (int \A)))))


(defn remove-nodes
  [graph nodes]
  (if (empty? nodes)
    graph
    (recur (remove-node graph (first nodes))
           (rest nodes))))


(defn worker-available?
  [{:keys [workers num-workers] :as state}]
  (if (< (count workers) num-workers)
    true
    false))


(defn task-available?
  [{:keys [graph workers todo] :as state}]
  (if (< 0 (count todo))
    true
    false))


(defn next-task
  [todo offset]
  (let [sorted-todo (sort todo)]
    [(first sorted-todo) (time-needed (first sorted-todo) offset)]))


(defn assign-tasks-to-workers
  [{:keys [todo offset] :as state}]
  (if (or (zero? (count todo))
          (not (worker-available? state)))
    state
    (recur (-> state
               (update :workers #(conj % (next-task todo offset)))
               (update :todo rest)))))


(defn remove-done
  [{:keys [graph workers] :as state}]
  (let [done? #(zero? (second %))
        done (map first (filter done? workers))]
    (-> state
        (update :graph #(remove-nodes % done))
        (update :workers (partial remove done?)))))


(defn update-todo
  [{:keys [graph workers] :as state}]
  (let [available-tasks (set (walk-graph graph))
        doing (set (map first workers))
        new-tasks (vec (clojure.set/difference available-tasks doing))]
    (update state :todo #(distinct (concat % new-tasks)))))


(defn update-workers
  [{:keys [graph todo workers] :as state}]
  (let [update-one (fn [x] [(first x) (dec (second x))])]
    (update state :workers #(map update-one %))))


(defn next-second
  [{:keys [graph todo workers] :as state}]
  (-> state
      (update-workers)
      (remove-done)
      (update :elapsed-time inc)))


(defn part2 
  [{:keys [graph todo workers elapsed-time] :as state}]
  (if (and (empty? graph)
           (empty? todo)
           (empty? workers))
    elapsed-time
    (let [state (update-todo state)]
      (if (and (worker-available? state)
               (task-available? state))
        (recur (next-second (assign-tasks-to-workers state)))
        (recur (next-second state))))))
  
(time (part2 (init-state "./resources/day07/input.txt" 60 5)))

;;;=>1081
;;;"Elapsed time: 17267.733962 msecs" sorry about that


