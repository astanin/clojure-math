(ns clojure.math.geometry
  (:import [org.apache.commons.math3.geometry.euclidean.oned Vector1D]
           [org.apache.commons.math3.geometry.euclidean.twod Vector2D]
           [org.apache.commons.math3.geometry.euclidean.threed Vector3D]))


(defprotocol EuclideanVector
  (plus [x y]    "Calculates a sum of vectors x and y.")
  (minus [x y]   "Subtracts vector y from x.")
  (scale [x alpha] "Multiplies a vector x by a scalar alpha.")
  (dot [x y]     "Calculates a dot product beween two vectors.")
  (norm [x]      "Calculates an Euclidean norm of the vector.")
  (normalize [x] "Returns a vector of length 1 coaligned with the original vector x."))


(defprotocol HasCross
  "Vectors which support a cross product."
  (cross [x y] "Returns a cross product  between two three-dimensional vectors."))


(extend-type clojure.lang.Seqable
  EuclideanVector
  (plus [x y] (mapv + x y))
  (minus [x y] (mapv - x y))
  (scale [x alpha] (mapv * x (repeat alpha)))
  (dot [x y] (reduce + (mapv * x y)))
  (norm [x] (Math/sqrt (dot x x)))
  (normalize [x] (let [length (norm x)] (mapv / x (repeat length))))
  HasCross
  (cross [x y] (condp = (mapv count [x y])
                 [2 2] (let [[x1 x2] x
                             [y1 y2] y]
                         (- (* x1 y2) (* x2 y1)))
                 [3 3] (let [[x1 x2 x3] x
                             [y1 y2 y3] y]
                         [ (- (* x2 y3) (* x3 y2))
                           (- (* x3 y1) (* x1 y3))
                           (- (* x1 y2) (* x2 y1))])
                 (throw (IllegalArgumentException. "dimension must be 2 or 3")))))


(defprotocol Vectorizable
  "Data types convertible to Commons Math's Euclidean vectors."
  (to-vector [coll] "Converts a sequence to a Common Math's vector."))


(defmacro seqable-toArray
  "Derives a new class from base-class, and implements Sequable by
  using .toArray method of the parent."
  [base-class & init-args]
  `(proxy [~base-class clojure.lang.Seqable] [~@init-args]
     (seq [] (seq (.toArray ~'this)))))


(defn make-vector
  "Creates an Euclidean vector."
  ([^double x ^double y]
     (seqable-toArray Vector2D x y))
  ([^double x ^double y ^double z]
     (seqable-toArray Vector3D x y z)))


(extend-protocol Vectorizable
  clojure.lang.Seqable (to-vector [coll] (apply make-vector coll))
  Vector2D (to-vector [v] v)
  Vector3D (to-vector [v] v))


(extend-type (class (double-array 0))
  Vectorizable
  (to-vector [arr]
    (condp = (alength arr)
      2 (seqable-toArray Vector2D arr)
      3 (seqable-toArray Vector3D arr))))


(extend-type Vector2D
  EuclideanVector
  (plus [x y] (.add x (to-vector y)))
  (minus [x y] (.subtract x (to-vector y)))
  (scale [x alpha] (.scalarMultiply x alpha))
  (dot [x y] (.dotProduct x (to-vector y)))
  (norm [x] (.getNorm x))
  (normalize [x] (.normalize x))
  HasCross
  (cross [[x1 x2] [y1 y2]] (- (* x1 y2) (* x2 y1))))


(extend-type Vector3D
  EuclideanVector
  (plus [x y] (.add x (to-vector y)))
  (minus [x y] (.subtract x (to-vector y)))
  (scale [x alpha] (.scalarMultiply x alpha))
  (dot [x y] (.dotProduct x (to-vector y)))
  (norm [x] (.getNorm x))
  (normalize [x] (.normalize x))
  HasCross
  (cross [x y] (Vector3D/crossProduct x (to-vector y))))
