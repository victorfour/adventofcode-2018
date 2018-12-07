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
  This map has shape {\C [], \A \C}.
  In this example, \C has no dependency. \A depends on \C"
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
   (alg graph []))
  ([graph acc]
   (if (empty? graph)
     (apply str acc)
     (let [dep-free-nodes (walk-graph graph)
           next-graph (remove-node graph (first dep-free-nodes))]
       (recur next-graph (conj acc (first dep-free-nodes)))))))

(time (println (part1 (parse-data "./resources/day07/input.txt"))))
;;;=>AEMNPOJWISZCDFUKBXQTHVLGRY
;;; "Elapsed time: 608.031696 msecs"




