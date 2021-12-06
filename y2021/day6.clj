(ns day6
  (:require [utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.java.math :as math]
            [clojure.walk :as walk]
            [quil.core :as q]))

(do 
  (defn parse [input]
    (->> input
         u/parse-comma-separated-list
         (map parse-long)
         )
    )

  (parse (u/get-input 2021 6))
  
  )


;; part 1

(reduce
 )

(defn fish [timer days]
  
  )

(int (math/pow 2 (int (/ 80 6))))

(do
  
  (defn modelate [l]
    (merge
     {0 0 1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0}
     (frequencies l)))

  (def to
    {8 7
     7 6
     6 5
     5 4
     4 3
     3 2
     2 1
     1 0
     0 8})
  

  (defn next [l]
    {0 (get l 1)
     1 (get l 2)
     2 (get l 3)
     3 (get l 4)
     4 (get l 5)
     5 (get l 6)
     6 (+ (get l 7) (get l 0))
     7 (get l 8)
     8 (get l 0)}
    )
  
  (->> (parse #_"3,4,3,1,2"
              (u/get-input 2021 6))
       modelate


       (iterate next)
       (take (inc 256))
       last
       vals
       (reduce +)
       ;; (take 81)
       
       ;; last
       ;; count
       ))

362639



(->> (parse "3,4,3,1,2")
     (map (fn [n] (if (zero? n)
                    [6 8]
                    (dec n)))
          )
     flatten)
