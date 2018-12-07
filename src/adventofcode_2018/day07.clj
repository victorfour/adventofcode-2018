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

(time (println (part1 (parse-data "./resources/day07/input.txt"))))
;;;=>AEMNPOJWISZCDFUKBXQTHVLGRY
;;; "Elapsed time: 608.031696 msecs"


;;; part 2

(def example-data (parse-data "./resources/day07/example.txt"))
(def data (parse-data "./resources/day07/input.txt"))


(walk-graph example-data)
(def next-graph (remove-node example-data \C))

(print next-graph)
(walk-graph next-graph)


(def num-workers 5)
(def offset 0)


(defn worker-available?
  [worker-pool]
  (not= (count worker-pool) num-workers))


(defn time-needed
  [task]
  (println (str "time needed:" task))
  (+ offset (inc (- (int task) (int \A)))))


(defn remove-nodes
  [graph nodes]
  (if (empty? nodes)
    graph
    (recur (remove-node graph (first nodes))
           (rest nodes))))


(defn get-next-tasks
  [[worker-pool task-queue graph]]
  (let [new-tasks (walk-graph graph)
        next-graph (remove-nodes graph new-tasks)
        next-task-queue (vec (concat (vec task-queue) new-tasks))]
    [worker-pool next-task-queue next-graph]))


(defn add-task-to-pool
  [[worker-pool task-queue graph]]
  [(flatten (conj worker-pool (time-needed (first task-queue))))
   (rest task-queue)
   graph])
  

(defn fill-worker-pool
  [[worker-pool task-queue graph]]
   (if (worker-available? worker-pool)
     (if (empty? task-queue)
       (recur (get-next-tasks [worker-pool task-queue graph]))
       (recur (add-task-to-pool [worker-pool task-queue graph])))
     [worker-pool task-queue graph]))

;; fn next-second : (graph, task-queue, worker-pool, elapsed-time) -> (all state)
;; dec all values in worker pool
;; remove all tasks that get to zero from worker-pool
;; inc elapsed time
;(defn next-second
;  [pool queue graph elapsed-time]
;  (let [next-pool (map #(
  
(defn next-second
  [pool queue graph elapsed-time]
  [(remove #(= % 0) (map dec pool))
   queue
   graph
   (inc elapsed-time)])

 

(defn timestep
  ([graph worker-pool task-queue]
   (timestep [graph worker-pool task-queue 0]))
  ([[graph worker-pool task-queue elapsed-time]]
   (if (and (empty? graph)
            (empty? task-queue)
            (empty? worker-pool))
     elapsed-time
     (if (worker-available? worker-pool)
       (let [[next-pool next-queue next-graph] (fill-worker-pool [worker-pool task-queue graph])]
         (recur (next-second next-pool next-queue next-graph elapsed-time)))
       (recur (next-second worker-pool task-queue graph elapsed-time))))))
  
(timestep example-data [] [])

