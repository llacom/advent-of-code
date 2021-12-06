(ns day5
  (:require [utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.java.math :as math]
            [clojure.walk :as walk]
            [quil.core :as q]))

;; (defn setup []
;;   (q/frame-rate 1)                    ;; Set framerate to 1 FPS
;;   (q/background 200))                 ;; Set the background colour to
;; ;; a nice shade of grey.
;; (defn draw []
;;   (q/stroke (q/random 255))             ;; Set the stroke colour to a random grey
;;   (q/stroke-weight (q/random 10))       ;; Set the stroke thickness randomly
;;   (q/fill (q/random 255))               ;; Set the fill colour to a random grey

;;   (let [diam (q/random 100)             ;; Set the diameter to a value between 0 and 100
;;         x    (q/random (q/width))       ;; Set the x coord randomly within the sketch
;;         y    (q/random (q/height))]     ;; Set the y coord randomly within the sketch
;;     (q/ellipse x y diam diam)))         ;; Draw a circle at x y with the correct diameter

;; (q/defsketch example                  ;; Define a new sketch named example
;;   :title "Oh so many grey circles"    ;; Set the title of the sketch
;;   :settings #(q/smooth 2)             ;; Turn on anti-aliasing
;;   :setup setup                        ;; Specify the setup fn
;;   :draw draw                          ;; Specify the draw fn
;;   :size [323 200])                    ;; You struggle to beat the golden ratio

;; part 1

(defn parse [input]
  (->> input
       u/parse-newline-separated-list
       #_(map #(str/split % #" -> "))
       (map (fn [s]
              (let [[_ x1 y1 x2 y2] (re-find #"([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)" s)]
                {:x1 (parse-long x1)
                 :y1 (parse-long y1)
                 :x2 (parse-long x2)
                 :y2 (parse-long y2)})))
       )
  )

(do

  (defn print-board [cells]
    (let [cells-flat (apply concat (map second cells))
          max-x (inc (apply max (map first cells-flat)))
          max-y (inc (apply max (map second cells-flat)))
          board (vec (repeat max-y
                             (vec (repeat max-x 0))))
          ]

      (println "------------------------------")
      (->> cells-flat
           (reduce (fn [board [x y]]
                     (update-in board [y x] inc))
                   board)
           (map #(replace {0 \.} %))
           (map #(apply str (interpose " " %)))
           (mapv println))

      cells))

  (->> (parse
        (u/get-input 2021 5)
        #_"0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

       (map (juxt
             identity
             (fn [{:keys [x1 x2 y1 y2]}]

               (if (or (= x1 x2)
                       (= y1 y2))
                 (for [x (apply range (update (vec (sort [x1 x2])) 1 inc))
                       y (apply range (update (vec (sort [y1 y2])) 1 inc))]
                   [x y])

                 (cond
                   (and (< x1 x2)
                        (< y1 y2))
                   (map vector
                        (range x1 (inc x2))
                        (range y1 (inc y2))
                        #_#_(if (< x1 x2)
                              (range x1 (inc x2))
                              (range x2 (inc x1)))
                        (if (< y1 y2)
                          (range y1 (inc y2))
                          (range y2 (inc y1))))

                   

                   (and (< x2 x1)
                        (< y2 y1))
                   (map vector
                        (range x2 (inc x1))
                        (range y2 (inc y1)))

                   (and (< x1 x2)
                        (< y2 y1))
                   (map vector
                        (range x1 (inc x2))
                        (reverse (range y2 (inc y1))))

                   (and (< x2 x1)
                        (< y1 y2))
                   (map vector
                        (range x2 (inc x1))
                        (reverse (range y1 (inc y2))))
                   
                   )
                 
                 ))))

       ;; print-board
       ;; (remove (comp (fn [{:keys [x1 x2 y1 y2]}]
       ;;                 (or (= x1 x2)
       ;;                     (= y1 y2)))
       ;;               first))

       (map second)
       (apply concat)
       (group-by identity)
       vals
       (map count)
       (filter #(> % 1))
       count
       
       ))
