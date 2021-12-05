(ns day2
  (:require [utils :as u]
            [clojure.string :as str]))

;; part 1

(defn parse [input]
  (->> input
       u/parse-newline-separated-list
       (map (fn [s]
              (let [[a b] (str/split s #" ")]
                [(keyword a) (parse-long b)])))))

(def inst-fn
  {:forward (fn [o n] (update o :horizontal + n))
   :down    (fn [o n] (update o :depth + n))
   :up      (fn [o n] (update o :depth - n))})

((inst-fn :forward) {:horizontal 10} 13)

(reduce
 (fn [o [inst n]]
   ((inst-fn inst) o n))
 {:horizontal 0 :depth 0}
 (parse "forward 5
down 5
forward 8
up 3
down 8
forward 2"))

(reduce
 (fn [o [inst n]]
   ((inst-fn inst) o n))
 {:horizontal 0 :depth 0}
 (parse (u/get-input 2021 2)))
;; {:horizontal 1998, :depth 741}
(* 1998 741) ;; 1480518


;; part 2

(def inst-fn
  {:forward (fn [o n] (-> o
                          (update :horizontal + n)
                          (update :depth + (* (:aim o) n))))
   :down    (fn [o n] (update o :aim + n))
   :up      (fn [o n] (update o :aim - n))})

(def initial-pos {:horizontal 0
                  :depth 0
                  :aim 0})

(reductions
 (fn [o [inst n]]
   ((inst-fn inst) o n))
 initial-pos
 (parse "forward 5
down 5
forward 8
up 3
down 8
forward 2"))

(reduce
 (fn [o [inst n]]
   ((inst-fn inst) o n))
 initial-pos
 (parse (u/get-input 2021 2)))
;; {:horizontal 1998, :depth 642047, :aim 741}
(* 1998 642047) ;; 1282809906
