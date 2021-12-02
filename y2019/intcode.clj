(ns y2019.intcode
  (:require [hato.client :as hato]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.java.math :as math]
            [clojure.walk :as walk]

            [utils :as u]))

(defn parse-program [input]
  (->> input
       u/parse-comma-separated-list
       (mapv u/parse-integer)))

(def day2-input (->> (u/get-input 2)
                     (parse-program)))

(do
  (def opcodes
    {1 [3 (fn [p a1 a2 a3] {:action :set
                            :val (+ (p a1)
                                    (p a2))
                            :addr a3})]
     2 [3 (fn [p a1 a2 a3] {:action :set
                            :val (* (p a1)
                                    (p a2))
                            :addr a3})]
     
     99 [0 (fn [_p] {:action :halt})]
     })
  
  (defn run [i program]
    (let [op (get program i)
          [n-args op-fn] (opcodes op)
          args (take n-args (drop (inc i) program))
          op-res (apply op-fn
                        program
                        (take n-args (drop (inc i) program)))
          {:keys [action val addr]} op-res]
      #_(println (/ i 4) "op" op args op-res)
      (case action
        :halt program
        :set  (recur (+ i 1 n-args) (assoc program addr val)))))

  (defn speak [program noun verb]
    (first (run 0 (-> program
                      (assoc 1 noun)
                      (assoc 2 verb)))))

  ;; day 2 bis
  #_(filter (comp #{19690720} first)
            (for [noun (range (count day2-input))
                  verb (range (count day2-input))]
              [(speak day2-input noun verb) noun verb (+ (* noun 100) verb)]
              ))

  #_(run 0 (parse-program "1,0,0,0,99"))
  [(= (first (run 0 (-> day2-input
                        (assoc 1 12)
                        (assoc 2 2))))
      3895705)
   
   (= (speak day2-input 12 2) 3895705)])

(def day5-input (parse-program (u/get-input 5)))

(do
  (defn parse-op [op]
    {:op (mod op 100)
     :modes [(mod (int (/ op 100))   10)
             (mod (int (/ op 1000))  10)
             (mod (int (/ op 10000)) 10)]})

  (defn val [program arg mode]
    (if (= mode 0)
      (program arg)
      arg))
  
  (def opcodes
    {1 [3 (fn [p [a1 a2 a3] [m1 m2]] {:action :set
                                      :val (+ (val p a1 m1)
                                              (val p a2 m2))
                                      :addr a3})]
     2 [3 (fn [p [a1 a2 a3] [m1 m2]] {:action :set
                                      :val (* (val p a1 m1)
                                              (val p a2 m2))
                                      :addr a3})]

     3 [1 (fn [p [a1] _] {:action :input
                          :addr a1})]

     4 [1 (fn [p [a1] [m1]] {:action :output
                             :val (val p a1 m1)})]

     ;; Opcode 5 is jump-if-true: if the first parameter is non-zero,
     ;; it sets the instruction pointer to the value from the second parameter.
     ;; Otherwise, it does nothing.
     5 [2 (fn [p [a1 a2] [m1 m2]] (if (zero? (val p a1 m1))
                                    {:action :noop}
                                    {:action :jump
                                     :addr   (val p a2 m2)}))]
     
     ;; Opcode 6 is jump-if-false: if the first parameter is zero,
     ;; it sets the instruction pointer to the value from the second parameter.
     ;; Otherwise, it does nothing.
     6 [2 (fn [p [a1 a2] [m1 m2]] (if (zero? (val p a1 m1))
                                    {:action :jump
                                     :addr   (val p a2 m2)}
                                    {:action :noop}))]
     
     ;; Opcode 7 is less than: if the first parameter is less than
     ;; the second parameter, it stores 1 in the position given by the third parameter.
     ;; Otherwise, it stores 0.
     7 [3 (fn [p [a1 a2 a3] [m1 m2]] {:action :set
                                      :val (if (< (val p a1 m1)
                                                  (val p a2 m2))
                                             1 0)
                                      :addr a3})]
     
     ;; Opcode 8 is equals: if the first parameter is equal to the
     ;; second parameter, it stores 1 in the position given by the
     ;; third parameter. Otherwise, it stores 0.
     8 [3 (fn [p [a1 a2 a3] [m1 m2]] {:action :set
                                      :val (if (= (val p a1 m1)
                                                  (val p a2 m2))
                                             1 0)
                                      :addr a3})]
     
     99 [0 (fn [_p _args _modes] {:action :halt})]
     })

  (defn run [program input]
    (loop [i 0
           program program
           res []
           input input
           output []]
      (let [{:keys [op modes]} (parse-op (get program i))
            [n-args op-fn] (opcodes op)
            args (take n-args (drop (inc i) program))
            op-res (op-fn program args modes)
            {:keys [action val addr]} op-res
            
            res (conj res op-res)
            i   (+ i 1 n-args)]
        #_(println op "args" args modes "op-res" op-res)
        (case action
          :halt {:output output :program program}
          :set  (recur i
                       (assoc program addr val)
                       res input output)
          :input (recur i
                        (assoc program addr (first input))
                        res
                        (rest input)
                        output)
          :output (recur i program res input
                         (conj output val))
          :jump (recur addr program res input output)
          :noop (recur i program res input output)))))

  #_(run 0 (parse-program "1,0,0,0,99"))
  [(= (first (:program (run (-> day2-input (assoc 1 12) (assoc 2 2)) [])))
      3895705)
   
   #_(parse-op 1002)

   (= (:output (run
                 (parse-program "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99")
                 [100]))
      
      [1001])
   ]

  ;; (:output (run day5-input [5])) ;; [14340395]
  
  )
