(ns y2019.core
  (:require [hato.client :as hato]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.java.math :as math]
            [clojure.walk :as walk]))

(defonce get-input
  (memoize (fn [day]
             (:body (hato/get (format "https://adventofcode.com/2019/day/%s/input" day)
                              {:headers {"cookie" (str/trim (slurp ".session"))}})))))

(defn parse-newline-separated-list [input]
  (str/split-lines input))

(defn parse-comma-separated-list [input]
  (str/split (str/trim input) #","))

(defn parse-integer [s]
  (Integer/parseInt s))

(def day1-input (->> (get-input 1)
                     (parse-newline-separated-list)
                     (map parse-integer)))

(reduce +
        (map (fn [n] (-> n (/ 3) int (- 2)))
             day1-input))

(defn fuel [n]
  (let [res (-> n (/ 3) int (- 2))]
    (if (pos? res)
      res
      0)))

(defn total-fuel [n]
  (reduce + (take-while pos? (iterate fuel (fuel n)))))

(reduce + (map total-fuel day1-input)) ;; 4959709

(defn parse-program [input]
  (->> input
       parse-comma-separated-list
       (mapv parse-integer)))

(def day3-input
  (->> (get-input 3)
       (parse-newline-separated-list)
       (map (fn [input]
              (->> input
                   (parse-comma-separated-list)
                   (map (fn [s] [(first s) 
                                 (parse-integer (apply str (drop 1 s)))])))))
       ))


(defn total-fuel [n]
  (reduce + (take-while pos? (iterate fuel (fuel n)))))


(do
  (defn positions [dirs]
    (->> dirs
         (map (fn [[dir n]]
                (repeat n
                        (case dir
                          \R [ 1  0]
                          \L [-1  0]
                          \U [ 0 -1]
                          \D [ 0  1]))))
         (apply concat)
         (reductions (fn [[x y] [x' y']]
                       [(+ x x')
                        (+ y y')])
                     [0 0])
         #_rest))

  (defn manhattan-distance [[x y]]
    (+ (Math/abs x)
       (Math/abs y)))

  ;; day 3 bis
  #_(let [[i1 i2] day3-input
          pos1 (positions i1)
          pos2 (positions i2)
          cross (set/intersection (set pos1)
                                  (set pos2))
          
          i-pos1 (map-indexed vector (positions i1))
          i-pos2 (map-indexed vector (positions i2))
          ]

      (->> (for [[d1 pos1] (filter (comp #(cross %) second) i-pos1)
                 [d2 pos2] (filter (comp #(cross %) second) i-pos2)
                 :when (= pos1 pos2)]
             [(+ d1 d2) d1 d2 pos1 (manhattan-distance pos1)])
           (sort-by first)
           #_(sort-by last) ; manhattan
           )
      )

  )

(def day4-input
  (map (fn [x] (->> (vec x)
                    (map str)
                    (map parse-integer)))
       (str/split (str/trim (get-input 4)) #"-")))

(do 
  (defn to-num [arr]
    (->> (reverse arr)
         (map-indexed (fn [i n] (* (Math/pow 10 i) n)))
         (reduce +)
         int))

  (let [[a b] day4-input]
    [a b (to-num a)]
    (->> (range (to-num a) (to-num b))
         (map (fn [x] (->> (str x)
                           vec
                           (map str)
                           (map parse-integer))))
         ;; (map sort)
         (filter (fn [x] (= x (sort x))))
         ;; (partition-by =)
         (map (fn [x] (->> (partition-by identity x)
                           (map count))))
         
         (filter (fn [x] (some #(= % 2) x)))
         count
         )
    #_(take 10
            (combo/combinations (range 10) (count b)))

    #_(take 10
            (for [arr (repeat (count b) (range 10))
                  ]
              arr))))

(defn parse-day6 [input]
  (update-vals
   (group-by first
             (->> (parse-newline-separated-list input)
                  (map #(str/split % #"\)"))))
   (fn [v] (map second v))))

(def day6-input
  (parse-day6 (get-input 6)))

(def day6-sample (parse-day6 "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L"))

(def day6-sample-bis (parse-day6 "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN"))

(do 
  (defn make-tree [root level values]
    (let [childs (get values root)]
      {:node root
       :level level
       :childs (map #(make-tree % (inc level) values) childs)}))

  (defn count-orbits [tree level total]
    (let [childs (:childs tree)]
      (if (seq childs)
        (+ total (* level (count childs)))
        total)))

  (defn has-both [tree]
    (let [nodes (->> tree
                     (tree-seq (comp seq :childs) :childs)
                     (map :node)
                     set)]
      nodes
      (and (nodes "YOU")
           (nodes "SAN"))))

  (->> (make-tree "COM" 0
                  #_day6-sample
                  day6-sample-bis
                  #_day6-input)
       
       (tree-seq (comp seq :childs) :childs)
       (filter has-both)
       last

       ((fn [tree]
          (let [base-level (:level tree)
                you-level (->> (filter (comp #(= % "YOU") :node)
                                       (tree-seq (comp seq :childs) :childs tree))
                               first
                               :level
                               dec)
                san-level (->> (filter (comp #(= % "SAN") :node)
                                       (tree-seq (comp seq :childs) :childs tree))
                               first
                               :level
                               dec)]
            [base-level
             you-level
             san-level
             (+ (- you-level base-level)
                (- san-level base-level))]
            )))
       
       ;; (map :node)
       ;; (map (fn [x] (update x :childs count)))
       ;; (map :level)
       ;; (reduce + 0)
       ))
