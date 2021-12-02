(ns utils
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
