(ns clj-mackay.hamming
  (:require [incanter.core :as incanter])
  (:use [clj-mackay.util :only (zero-vector unit-vector matrix-mod2)]
	[clj-mackay.bitcoll :only (as-seq size set-bit get-bit new-coll)])
  (:import (cern.colt.matrix.tbit BitVector)))

;;;;;;;;;;;;;;;;;;;;; (7, 4) Hamming Codes ;;;;;;;;;;;;;;;;;;;;;
;; See page 24 of mackay for the reason behind the following matrix
;; definitions.

(def parity-matrix
  (incanter/matrix
   [[1 1 1 0]
    [0 1 1 1]
    [1 0 1 1]]))

(def generator-transpose-matrix
  (incanter/bind-rows (incanter/identity-matrix 4) parity-matrix))

(def parity-check-matrix
  (incanter/bind-columns parity-matrix (incanter/identity-matrix 3)))

(def decoding-matrix
  (incanter/matrix
   [[1 0 0 0 0 0 0]
    [0 1 0 0 0 0 0]
    [0 0 1 0 0 0 0]
    [0 0 0 1 0 0 0]]))

(defn hamming-code-encode
  "Encode the bit collection using (7,4) hamming code. Size of bitcoll must
   be a multiple of 4 bits."
  [bitcoll]
  (let [encoded (->> (as-seq bitcoll)
		     (map #(if % 1 0))
		     (partition 4)
		     (map incanter/matrix)
		     (map #(incanter/mmult generator-transpose-matrix %))
		     (map #(matrix-mod2 %))
		     (apply concat))
	output (new-coll bitcoll (count encoded))]
    (dorun (map-indexed (fn [index bit]
			  (when (== bit 1)
			    (set-bit output index))) encoded))
    output))

(def syndrome-to-noisevector
     {[0 0 0] (zero-vector 7)
      [0 0 1] (unit-vector 7 7)
      [0 1 0] (unit-vector 6 7)
      [0 1 1] (unit-vector 4 7)
      [1 0 0] (unit-vector 5 7)
      [1 0 1] (unit-vector 1 7)
      [1 1 0] (unit-vector 2 7)
      [1 1 1] (unit-vector 3 7)})

(defn hamming-code-decode
  "Decode the bitvector using syndrome decoding. This method splits
   the input into blocks of 7 bits and then decodes them block by block
   and joins them together. Since incanter doesn't have support for bit
   matrices, a double matrix is used and mod2 is done after every matrix
   multiplication to simulate an xor based addition in a bit-matrix."
  [bitcoll]
  (let [as-matrices (->> bitcoll
			 as-seq
			 (map #(if % 1 0))
			 (partition 7)
			 (map incanter/matrix))
	syndromes (->> as-matrices
		       (map #(incanter/mmult parity-check-matrix %))
		       (map #(matrix-mod2 %)))
	noise-vectors (map syndrome-to-noisevector syndromes)
	decoded (->> as-matrices
		     (map incanter/plus noise-vectors)
		     (map #(matrix-mod2 %))
		     (map incanter/matrix)
		     (map #(incanter/mmult decoding-matrix %))
		     (map #(matrix-mod2 %))
		     (apply concat))
	output (new-coll bitcoll (* 4/7 (size bitcoll)))]
    (dorun
     (map-indexed (fn [index bit]
		    (if (== bit 1)
		      (set-bit output index))) decoded))
    output))

;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use 'clojure.test)

(deftest hamming-code-test
  (let [bv (-> (BitVector. 16)
	       (set-bit 1)
	       (set-bit 6)
	       (set-bit 11))]
    (println "BV: " bv)
    (let [encoded (hamming-code-encode bv)
	  decoded (hamming-code-decode encoded)]
      (println "Encoded:" encoded)
      (println "Decoded:" decoded)
      (is (= decoded bv))
      (testing "Single bit error correction on the first and third blocks"
	(let [buggy (-> encoded
			(set-bit 3)
			(set-bit 15))
	      corrected (hamming-code-decode buggy)]
	  (println "Buggy:" )
	  (println "Error corrected:" corrected)
	  (is (= corrected bv)))))))

(defn test-ns-hook []
  (hamming-code-test))