(ns utils
  (:require [hato.client :as hato]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.java.math :as math]
            [clojure.walk :as walk]))

(defonce get-input
  (memoize (fn [year day]
             (:body (hato/get (format "https://adventofcode.com/%s/day/%s/input"
                                      year day)
                              {:headers {"cookie" (str/trim (slurp ".session"))}})))))



(defn parse-newline-separated-list [input]
  (str/split-lines input))

(defn parse-comma-separated-list [input]
  (str/split (str/trim input) #","))

(defn parse-space-separated-list [input]
  (str/split (str/trim input) #"\s+"))

(defn parse-integer [s]
  (Integer/parseInt s))
