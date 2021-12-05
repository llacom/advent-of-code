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
          board (map #(if (numbers %) nil %) board)
          cols (partition 5 board)
          rows (apply map vector cols)
          diagonals [(for [i [0 6 12 18 24]] (nth board i))
                     (for [i [4 8 12 16 20]] (nth board i))]

          check-row (fn [row] (empty? (remove nil? row)))
          ]
      [;; board cols rows diagonals
       ;; (concat cols rows diagonals)
       (some true?
             (map check-row
                  (concat cols rows #_diagonals)))

       board

       ]
      
      ))
  

  (def res (let [[numbers boards] (parse (u/get-input 2021 4)
                                         #_"7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")]
             (for [i (range (count numbers))
                   [board-n board] (map-indexed vector boards)
                   :let [numbers (take i numbers)
                         [won? board] (check-board numbers board)]]
               {:won won?
                :board-n board-n
                :i i
                :last-number (last numbers)
                :sum (reduce + (remove nil? board))})
             ))
  )

(->> res
     (filter (comp true? :won))
     #_(sort-by :board-n)
     (group-by :board-n)
     vals
     (map #(sort-by :i %))
     (map first)
     (sort-by :i)

     reverse
     )

;; first part
(* 39 693) 27027

;; second part
(* 87 425) 36975
