(ns clojure.math.internal.cmath3
  "Internal interface to Commons Math3 library functions and types."
  (:import [org.apache.commons.math3.geometry.euclidean.oned Vector1D]
           [org.apache.commons.math3.geometry.euclidean.twod Vector2D]
           [org.apache.commons.math3.geometry.euclidean.threed Vector3D]))


(defprotocol Vectorizable
  "Data types convertible to Commons Math's Euclidean vectors."
  (to-vector [coll] "Converts a sequence to a Common Math's Euclidean vector (2D or 3D)."))


(defmacro derive-seqable
  "Derives a new class from base-class, and implements Seqable by
  using .toArray method of the parent."
  [base-class & init-args]
  `(proxy [~base-class clojure.lang.Seqable] [~@init-args]
     (seq [] (let [~(with-meta 'this {:tag `~base-class}) ~'this]
               (seq (.toArray ~'this))))))


(defn make-vector
  "Creates an Commons-Math's Euclidean vector (2D or 3D)."
  ([^double x ^double y]
     (derive-seqable Vector2D x y))
  ([^double x ^double y ^double z]
     (derive-seqable Vector3D x y z)))


(extend-protocol Vectorizable
  clojure.lang.Seqable (to-vector [coll] (apply make-vector coll))
  Vector2D (to-vector [v] v)
  Vector3D (to-vector [v] v))


(extend-type (class (double-array 0))
  Vectorizable
  (to-vector [^doubles arr]
    (condp = (alength ^doubles arr)
      2 (derive-seqable Vector2D arr)
      3 (derive-seqable Vector3D arr))))


;;; Disable generated codox API documentation for some vars
(alter-meta! #'Vectorizable assoc :no-doc true)
(alter-meta! #'derive-seqable assoc :no-doc true)
