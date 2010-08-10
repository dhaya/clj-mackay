(ns clj-mackay.util
  (:require [incanter.core :as incanter]))

(defn unit-vector
  "Unit vector e(i)"
  [i dimension]
  (-> (repeat dimension 0)
      vec
      (assoc (dec i) 1)
      (incanter/matrix)))

(defn zero-vector
  "Zero vector of length dim"
  [dimension]
  (-> (repeat dimension 0)
      (incanter/matrix)))

(defn matrix-mod2
  "Does a mod2 on the elements of the matrix. Used to simulate sum using
   bit-xor, since incanter doesn't have bit-matrices."
  [m]
  (incanter/matrix-map #(mod (int %) 2) m))