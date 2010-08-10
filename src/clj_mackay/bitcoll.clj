(ns clj-mackay.bitcoll
  (:import (cern.colt.matrix.tbit BitVector)))

(defprotocol BitCollection
  "Protocols for concrete bit collections that can be used with this library.
   BitVector from parallel colt is used for testing here, but any concrete
   type including BitSet can be used as long as it is extended to support
   the protocol."
  (size [bits] "Number of bits in this collection")
  (set-bit [bits index] "Sets the bit at the index position")
  (get-bit [bits index] "Returns boolean indicating the bit position.")
  (new-coll [bits size] "Factory method returning a new bit collection of the same type. All bits are initialy un-set"))

(extend-protocol BitCollection
  cern.colt.matrix.tbit.BitVector
    (size [this] (.size this))
    (get-bit [this index] (.get this index))
    (set-bit [this index]
	     (.set this index)
	     this)
    (new-coll [this size] (BitVector. size))
  nil
    (size [this] 0)
    (get-bit [this index] (throw (NullPointerException.)))
    (set-bit [this index] (throw (NullPointerException.)))
    (new-coll [this size] nil))

(defn as-seq
  "Needed since clojure.lang.Seqable is still not a protocol, hence it would
   be possible to just extend that for the concrete type."
  [bitcoll]
  (->> (range (size bitcoll))
       (map #(get-bit bitcoll %))))