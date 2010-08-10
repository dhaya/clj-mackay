(ns clj-mackay.repetition
  (:use clj-mackay.bitcoll)
  (:import (cern.colt.matrix.tbit BitVector)))

(defn repetition-code-encode
  "Simple repetition code that repeats every bit n times."
  [bitcoll n]
  (let [size (size bitcoll)
	ret (new-coll bitcoll (* n size))
	indices (for [i (range size)
		      j (range (* n i) (* n (inc i)))]
		  [i j])]
    (doseq [[i j] indices]
      (when (get-bit bitcoll i)
	(set-bit ret j)))
    ret))

(declare majority-ones?)
(defn repetition-code-decode
  [bitcoll n]
  (let [ret (new-coll bitcoll (/ (size bitcoll) n))]
    (doseq [i (range (size ret))]
      (let [repeat-indices (range (* n i) (* n (inc i)))]
	(when (majority-ones? bitcoll repeat-indices)
	  (set-bit ret i))))
    ret))

(defn- majority-ones?
  [bitcoll indices]
  (let [numones (->> indices
		     (map #(get-bit bitcoll %))
		     (filter identity)
		     count)]
    (> numones (/ (count indices) 2))))

;;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use 'clojure.test)

(deftest repetition-code-test
  (let [bv (BitVector. 4)]
    (set-bit bv 0)
    (set-bit bv 3)
    (println "BV:" bv)
    (let [encoded (repetition-code-encode bv 3)
	  decoded (repetition-code-decode encoded 3)]
      (println "Encoded:" encoded)
      (println "Decoded:" decoded)
      (is (= 12 (.size encoded)))
      (is (= 4 (.size decoded)))
      (is (= decoded bv))
      (testing "Single bit error correction"
	(let [buggy (set-bit encoded 4)
	      decoded (repetition-code-decode buggy 3)]
	  (is (= decoded bv)))))))

(defn test-ns-hook []
  (repetition-code-test))