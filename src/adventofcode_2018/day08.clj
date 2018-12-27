(ns adventofcode-2018.day08
  (:require [clojure.string :as str]))

;;; part 1

(def exampletxt "./resources/day08/example.txt")
(def inputtxt "./resources/day08/input.txt")
(def one-nodetxt "./resources/day08/one-node.txt")

(defn parse-data
  [filename]
  (-> (slurp filename)
      (str/trim)
      (str/split #" ")
      (->> (map #(Long/parseLong %)))))


(defn part1
  [input]
  (let [num-children (first input)
        num-elems (second input)
        tail (drop 2 input)]
    (loop [n-child num-children
           cur-input tail
           acc 0]
      (if (zero? n-child)
        [(drop num-elems cur-input) (+ acc (reduce + (take num-elems cur-input)))]
        (let [[next-input next-acc] (part1 cur-input)]
          (recur (dec n-child)
                 next-input
                 (+ acc next-acc)))))))

(println (part1 (parse-data inputtxt)))
;;=>[() 47112]


;;; part 2


(defn rec-build-tree
  [input]
  (let [num-children (first input)
        num-elems (second input)
        tail (drop 2 input)]
    (loop [n-child num-children
           cur-input tail
           acc []]
      (if (zero? n-child)
        [(drop num-elems cur-input) (vec (concat acc (vec (take num-elems cur-input))))]
        (let [[next-input next-acc] (rec-build-tree cur-input)]
          (recur (dec n-child)
                 next-input
                 (conj acc next-acc)))))))

(defn build-tree
  [input]
  (second (rec-build-tree input)))


(defn get-node-value
  [node]
  (if (not (some sequential? node))
    (apply + node)
    (let [children (filter sequential? node)
          metadata (remove sequential? node)]
      (some->> metadata
               (map dec)
               (map #(nth children % nil))
               (map get-node-value)
               (flatten)
               (apply +)))))
               

(println (get-node-value (build-tree (parse-data inputtxt))))


