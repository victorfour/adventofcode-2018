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
;;;=>AEMNPOJWISZCDFUKBXQTHVLGRY
;;; "Elapsed time: 608.031696 msecs"


;;; part 2

(def example-data (parse-data "./resources/day07/example.txt"))
(def data (parse-data "./resources/day07/input.txt"))


;(take 5 (iterate (walk-graph example-data)))

(def next-graph (remove-node example-data \C))

(print next-graph)
(walk-graph next-graph)



(def num-workers 2)
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
  [[graph worker-pool task-queue]]
  (let [new-tasks (walk-graph graph)
        next-graph (remove-nodes graph new-tasks); don't do this
        next-task-queue (vec (concat (vec task-queue) new-tasks))]
    [next-graph worker-pool next-task-queue]))


(defn add-task-to-pool
  [[graph worker-pool task-queue]]
  [graph
   (vec (flatten (conj worker-pool (time-needed (first task-queue)))))
   (rest task-queue)])
  

(defn fill-worker-pool
  [[graph worker-pool task-queue]]
  (if (worker-available? worker-pool)
    (if (empty? task-queue)
      (recur (get-next-tasks [graph worker-pool task-queue]))
      (recur (add-task-to-pool [graph worker-pool task-queue])))
    [graph worker-pool task-queue]))

  
(defn next-second
  [graph pool queue elapsed-time]
  (let [next-pool (remove #(= % 0) (mapv dec pool))
        next-elapsed (inc elapsed-time)]
    [graph next-pool queue next-elapsed]))

;  [graph
;   (remove #(= % 0) (mapv dec pool))
;   queue
;   (inc elapsed-time)])


;;; TODO : don't put a task in worker-pool if its dependencies are not met..
;;; idea for fix: let `next-second` handle node removal
;;; worker-pool should feature names of tasks 
(defn timestep
  [[graph worker-pool task-queue elapsed-time]]
  (println (str "graph:" graph "|empty?:" (empty? graph)))
  (println (str "pool:" (seq worker-pool) "|empty?:" (empty? (seq worker-pool))))
  (println (str "queue:" task-queue "|empty?:" (empty? task-queue)))
  (println (str "elapsed:" elapsed-time))
  (println "------------------------")
  (if (and (empty? graph)
           (empty? task-queue)
           (empty? worker-pool))
    elapsed-time
    (if (worker-available? worker-pool)
      (if (and (empty? task-queue) (empty? graph))
        (recur (next-second graph worker-pool task-queue elapsed-time))
        (let [[next-graph next-pool next-queue] (fill-worker-pool [graph worker-pool task-queue])]
          (recur (next-second next-graph next-pool next-queue elapsed-time))))
      (recur (next-second graph worker-pool task-queue elapsed-time)))))

(println "------------------------")
(timestep [example-data [] [] 0])
