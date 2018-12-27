(ns adventofcode-2018.day13
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.core.matrix :as m]))


;;; part 1

(defn get-direction
  [char]
  (let [directions {\^ [-1 0]
                    \v [1 0]
                    \> [0 1]
                    \< [0 -1]}]
    (get directions char)))

(defn turn-left
  [char]
  (let [next-char {\^ \<
                  \v \>
                  \> \^
                  \< \v}]
    (get next-char char)))

(defn turn-right
  [char]
  (let [next-char {\^ \>
                  \v \<
                  \> \v
                  \< \^}]
    (get next-char char)))


(defn chose-direction
  [choice char]
  (case choice
    :left (turn-left char)
    :right (turn-right char)
    char))

(defn new-cart
  [i [j char]]
  {:position [i j] :char char :direction (get-direction char) :choice [:left :forward :right]})


(defn parse-carts
  [content]
  (loop [i 0
         carts []]
    (if-let [line (get content i)]
      (let [new-carts (->> (interleave (range (count line)) line)
                              (partition 2)
                              (filter #(#{\v \^ \< \>} (second %)))
                              (map vec)
                              (map (partial new-cart i))
                              (into []))]
        (if (< 0 (count new-carts))
          (recur (inc i)
                 (concat carts new-carts))
          (recur (inc i)
                 carts)))
      carts)))


(defn remove-carts
  [track carts]
  (let [substitute {\> \-
                    \< \-
                    \^ \|
                    \v \|}]
    (loop [track (mapv vec track)
           carts carts]
      (if (seq carts)
        (recur (assoc-in track (:position (first carts)) (get substitute (:char (first carts))))
               (rest carts))
        (mapv #(apply str %) track)))))

(defn replace-backslashes
  [track]
  (mapv #(str/replace % #"\\" ")") track))


(defn put-carts-on-track
  [{:keys [track carts]}]
  (loop [trac (mapv vec (replace-backslashes track))
         carts carts]
    (if (seq carts)
      (recur (assoc-in trac (:position (first carts)) (:char (first carts)))
             (rest carts))
      (mapv #(apply str %) trac))))

(defn print-state
  [state]
  (let [new-state (put-carts-on-track state)]
    (println (str "carts(" (count (:carts state)) "):\n" (:carts state)))
    (loop [s new-state]
      (when (seq s)
        (do
          (println (first s))
          (recur (rest s)))))))
        
    

(defn parse-data
  [filename]
  (let [content (vec (-> (slurp filename)
                         (str/split #"\n")))
        [width height] [(apply max (map count content)) (count content)]
        carts (parse-carts content)
        track (remove-carts content carts)]
    {:track track 
     :width width
     :height height
     :carts carts}))


(defn move-cart
  [cart]
  (when cart
    (assoc cart :position (m/add (:position cart) (get-direction (:char cart))))))


(defn rotate-cart
  [cart]
  (let [choice (first (:choice cart))]
    (-> cart
        (assoc :choice (concat (vec (rest (:choice cart))) [choice]))
        (assoc :char (chose-direction choice (:char cart))))))

(defn turn-back
  [cart]
  (let [next-char {\^ \<
                   \v \>
                   \< \^
                   \> \v}]
  (assoc cart :char (next-char (:char cart)))))

(defn turn-forward
  [cart]
  (let [next-char {\^ \>
                   \v \<
                   \< \v
                   \> \^}]
    (assoc cart :char (next-char (:char cart)))))


(defn update-directions
  [track cart]
  (case (get-in track (:position cart))
    \+ (rotate-cart cart)
    \\ (turn-back cart)
    \/ (turn-forward cart)
    cart))

(defn step
  [{:keys [carts track] :as state}]
  (let [to-move (sort-by :position carts)
        tmp-carts (mapv move-cart to-move)
        new-carts (mapv (partial update-directions track) tmp-carts)]
    (assoc state :carts new-carts)))



(defn find-collision
  [{:keys [carts]}]
  (loop [carts carts]
    (when (seq carts)
      (let [other-positions (set (map :position (rest carts)))]
        (if (other-positions (:position (first carts)))
          (:position (first carts))
          (recur (rest carts)))))))


  
(defn part1
  [filename]
  (let [initial-state (parse-data filename)]
    (loop [state initial-state]
      (if-let [location (find-collision state)]
        (reverse location)
        (recur (step state))))))

(time (println (part1 "./resources/day13/input.txt")))
;;=>(80 100)
;;"Elapsed time: 60.133355 msecs"


;;; part2

(defn remove-collisions
  [moved-cart moved rest-to-move]
  (when moved-cart
    (let [next-moved (remove #(= (:position moved-cart) (:position %)) moved)
          next-to-move (remove #(= (:position moved-cart) (:position %)) rest-to-move)]
      (if (or (some #(= (:position moved-cart) (:position %)) moved)
              (some #(= (:position moved-cart) (:position %)) rest-to-move))
        [next-moved next-to-move]
        [(conj next-moved moved-cart) next-to-move]))))


(defn stepped-step
  [{:keys [carts track] :as state}]
  (loop [moved '()
         to-move (sort-by :position carts)]
    (let [moved-cart (move-cart (first to-move))
          [next-moved next-to-move] (remove-collisions moved-cart moved (rest to-move))]
      (if moved-cart
        (recur next-moved next-to-move)
        (assoc state :carts (mapv (partial update-directions track) moved))))))


(defn part2
  [filename]
  (let [initial-state (parse-data filename)]
    (loop [state initial-state]
      (if (= 1 (count (:carts state)))
        (reverse (:position (first (:carts state))))
        (recur (stepped-step state))))))


(time (println (part2 "./resources/day13/input.txt")))
;;=>(16 99)
;;"Elapsed time: 903.29368 msecs"
