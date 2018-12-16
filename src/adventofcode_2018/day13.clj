(ns adventofcode-2018.day13
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))


;;; part 1

(defn parse-data
  [filename]
  (let [filename "./resources/day13/example.txt"
        content (-> (slurp filename)
                    (str/split #"\n"))
        ;[width height] [(apply max (map count content)) (count content)]
        ]
    (println content)
    ))
