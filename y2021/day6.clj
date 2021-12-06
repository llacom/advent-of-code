(ns day6
  (:require [utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.java.math :as math]
            [clojure.walk :as walk]
            [quil.core :as q]))

(defn parse [input]
  (->> input
       u/parse-comma-separated-list
       (map parse-long)))

(defn next [l]
  {0 (get l 1 0)
   1 (get l 2 0)
   2 (get l 3 0)
   3 (get l 4 0)
   4 (get l 5 0)
   5 (get l 6 0)
   6 (+ (get l 7 0) (get l 0 0))
   7 (get l 8 0)
   8 (get l 0 0)})

(->> (parse #_"3,4,3,1,2"
            (u/get-input 2021 6))
     frequencies
     (iterate next)
     (take (inc 256))
     last
     vals
     (reduce +))

;; 1639854996917
;; 362639
