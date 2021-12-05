(ns day1
  (:require [utils :as u]))

;; part 1

(->> (u/get-input 2021 1)
     u/parse-newline-separated-list
     (map u/parse-integer)
     (partition 2 1)
     (filter (fn [[a b]] (< a b)))
     count) ;; 1475

;; part 2

(->> (u/get-input 2021 1)
     u/parse-newline-separated-list
     (map u/parse-integer)
     (partition 3 1)
     (map #(reduce + %))
     (partition 2 1)
     (filter (fn [[a b]] (< a b)))
     count
     ) ;; 1516
