(ns day3
  (:require [utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.java.math :as math]
            [clojure.walk :as walk]))

;; part 1

(defn parse [input]
  (->> input
       u/parse-newline-separated-list
       (map vec)
       (map (fn [c]
              (map 
               #(case %
                  \0 -1
                  \1 1
                  ) c) ))
       #_(map (fn [s]
                (let [[a b] (str/split s #" ")]
                  [(keyword a) (parse-long b)])))))

(->> (parse (u/get-input 2021 3))
     (apply map vector)
     (map #(reduce + %))
     (map (fn [n] (if (pos? n)
                    0
                    1)))
     (apply str)
     )

"001000110110"

(* 3529 566) 1997414


;; part 2

(defn parse [input]
  (->> input
       u/parse-newline-separated-list
       (map vec)
       (map (fn [c]
              (map 
               #(case %
                  \0 -1
                  \1 1
                  ) c) ))
       #_(map (fn [s]
                (let [[a b] (str/split s #" ")]
                  [(keyword a) (parse-long b)])))))

(loop [l (parse (u/get-input 2021 3))]
  (if (= (count l) 1)
    l
    (let [most (->> l
                    (apply map vector)
                    (map #(reduce + %))
                    (map (fn [n] (if (pos? n)
                                   0
                                   1)))
                    (apply str)
                    )])
    ))

(defn most-common [n]
  (if (neg? n)
    -1
    1))

#_(loop [l (parse (u/get-input 2021 3))
         ns (range (count (first l)))]
    (let [n (first ns)
          mc (->> l
                  (map #(nth % n))
                  (reduce +)
                  most-common
                  )]
      (println mc (count l))
      (when (and (pos? (count l))
                 (seq ns))
        (recur
         (filter (fn [a] (= (first a) mc)) l)
         (rest ns)))
      )
    )

(loop [l (parse (u/get-input 2021 3))
       #_#_l (parse "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")
       ns (range (count (first l)))]
  (println "SEQ" (count l)
           (apply str (map {1 1 -1 0} (last l))))
  (when (> (count l) 1)
    (let [n (first ns)
          mc (->> l
                  (map #(nth % n))
                  (reduce +)
                  most-common
                  )

          mc (- mc)]
      (println n mc (count l))
      (when (and (pos? (count l))
                 (seq ns))
        (recur
         (filter (fn [a] (= (nth a n 10) mc)) l)
         (rest ns)))
      ))
  )
nil

(* 3573 289) 1032597

