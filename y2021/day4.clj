(ns day4
  (:require [utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.java.math :as math]
            [clojure.walk :as walk]))

;; part 1

(defn parse [input]
  (let [[numbers _ & boards] (->> input
                                  u/parse-newline-separated-list)
        numbers (map u/parse-integer (u/parse-comma-separated-list numbers))
        boards  (->> boards
                     (partition-by #(= % ""))
                     (remove #(= % '("")))
                     (map (partial str/join " "))
                     (map u/parse-space-separated-list)
                     (map #(map u/parse-integer %))
                     )]
    [numbers boards]))

(do
  (defn check-board [numbers board]
    (let [numbers (set numbers)
          cols (partition 5 board)
          rows (apply map vector cols)
          diagonals [(for [i [0 6 12 18 24]] (nth board i))
                     (for [i [4 8 12 16 20]] (nth board i))]

          check-row (fn [row] (empty? (remove numbers row)))
          ]
      [;; board cols rows diagonals
       (concat cols rows diagonals)
       
       ]
      (some true?
            (map check-row
                 (concat cols rows diagonals)))
      ))
  
  (let [[numbers boards] (parse (u/get-input 2021 4))]
    (filter
     (comp true? first)
     (for [i (range (count numbers))
           board boards
           :let [numbers (take i numbers)]]
       [(check-board numbers board) i]))))



