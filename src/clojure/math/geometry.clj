(ns clojure.math.geometry
  "Vector algebra."
  (:use clojure.math.internal.cmath3)
  (:require [clojure.math.internal.ejml :as ejml])
  (:import [org.apache.commons.math3.geometry.euclidean.oned Vector1D]
           [org.apache.commons.math3.geometry.euclidean.twod Vector2D]
           [org.apache.commons.math3.geometry.euclidean.threed Vector3D]
           [org.ejml.data DenseMatrix64F]))


(set! *unchecked-math* true)
(set! *warn-on-reflection* true)


(defprotocol EuclideanVector
  "Types with basic vector arithmetics: plus, minus, scale, dot.

   Known imlpementations:

     Clojure seqables,
     double arrays,
     Common-Math's Vector2D and Vector3D,
     EJML's DenseMatrix64F."
  (plus [x y]    "Calculates a sum of vectors x and y.")
  (minus [x y]   "Subtracts vector y from x.")
  (scale [x alpha] "Multiplies a vector x by a scalar alpha.")
  (dot [x y]     "Calculates a dot product beween two vectors."))


(defprotocol HasCross
  "Vectors which support a cross product: cross.

  Known implementations:

     Clojure seqables of length 3 or 2
     double arrays of length 3 or 2,
     Common-Math's Vector2D and Vector3D.

  For 2D vectors cross returns a scalar."
  (cross [x y] "Returns a cross product  between two three-dimensional vectors."))


(defprotocol HasEuclideanNorm
  "Defines an Euclidean norm and distance: norm, normalize, dist.

   Known imlpementations:

     Clojure seqables,
     double arrays,
     Common-Math's Vector2D and Vector3D,
     EJML's DenseMatrix64F."
  (norm [x]      "Calculates an Euclidean norm of the vector.")
  (normalize [x] "Returns a vector of length 1 coaligned with the original vector x.")
  (dist [x y]    "Euclidean distance between two vectors."))


(extend-type clojure.lang.Seqable
  EuclideanVector
  (plus [x y] (mapv + x y))
  (minus [x y] (mapv - x y))
  (scale [x alpha] (mapv * x (repeat alpha)))
  (dot [x y] (reduce + (mapv * x y)))
  HasEuclideanNorm
  (norm [x] (Math/sqrt (dot x x)))
  (normalize [x] (let [length (norm x)] (mapv / x (repeat length))))
  (dist [x y] (norm (minus x y)))
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


(defmacro map-double-array
  "Broadcasts an elementwise-operation to to the entire array. Returns a new array."
  ([f & arrs]
     (condp = (count arrs)
       1 (let [n (gensym "n")
               a (gensym "a")
               i (gensym "i")
               r (gensym "r")
               fun (gensym "fun")
               v (gensym "v")]
           `(let [~(with-meta a {:tag 'doubles}) ~(first arrs)
                  ~fun ~f
                  ~n (alength ~a)
                  ~(with-meta r {:tag 'doubles}) (double-array ~n)]
              (dotimes [~i (int ~n)]
                (let [~(with-meta v {:tag 'double}) (~fun (aget ~a ~i))]
                  (aset ~r ~i ~v)))
              ~r)))))


(extend-type (Class/forName "[D")
  EuclideanVector
  (plus [x y]
    (let [n (alength (doubles x))
          r (double-array n)]
      (dotimes [i n]
        (aset r i (+ (aget (doubles x) i) (aget (doubles y) i))))
      r))
  (minus [x y]
    (let [n (alength (doubles x))
          r (double-array n)]
      (dotimes [i n]
        (aset r i (- (aget (doubles x) i) (aget (doubles y) i))))
      r))
  (scale [x ^double alpha]
    (let [n (alength (doubles x))
          r (double-array n)]
      (dotimes [i n]
        (aset r i (double (* alpha (aget (doubles x) i)))))
      r))
  (dot [x y]
    (reduce +
            (let [n (alength (doubles x))
                  r (double-array n)]
              (dotimes [i n]
                (aset r i (* (aget (doubles x) i) (aget (doubles y) i))))
              r)))
  HasEuclideanNorm
  (norm [x]
    (Math/sqrt
     (reduce + (for [v (doubles x)] (* v v)))))
  (normalize [^doubles x]
    (let [denom (/ 1.0 (norm x))]
      (scale x denom)))
  (dist [x y]
    (norm (minus x y)))
  HasCross
  (cross [^doubles x ^doubles y]
    (cross (vec x) (vec y))))


(extend-type Vector2D
  EuclideanVector
  (plus [x ^Vector2D y] (.add x y))
  (minus [x ^Vector2D y] (.subtract x y))
  (scale [x ^double alpha] (.scalarMultiply x alpha))
  (dot [x ^Vector2D y] (.dotProduct x y))
  HasEuclideanNorm
  (norm [x] (.getNorm x))
  (normalize [x] (.normalize x))
  (dist [x y] (Vector2D/distance x y))
  HasCross
  (cross [[x1 x2] [y1 y2]] (- (* x1 y2) (* x2 y1))))


(extend-type Vector3D
  EuclideanVector
  (plus [x ^Vector3D y] (.add x y))
  (minus [x ^Vector3D y] (.subtract x y))
  (scale [x ^double alpha] (.scalarMultiply x alpha))
  (dot [x ^Vector3D y] (.dotProduct x y))
  HasEuclideanNorm
  (norm [x] (.getNorm x))
  (normalize [x] (.normalize x))
  (dist [x y] (Vector3D/distance x y))
  HasCross
  (cross [x ^Vector3D y] (Vector3D/crossProduct x y)))


(extend-type DenseMatrix64F
  EuclideanVector
  (plus [x ^DenseMatrix64F y]
    (ejml/add x y))
  (minus [x ^DenseMatrix64F y]
    (ejml/sub x y))
  (scale [x ^double alpha]
    (ejml/scalar-mult x alpha))
  (dot [x ^DenseMatrix64F y]
    (ejml/sum-elements (ejml/element-mult x y)))
  HasEuclideanNorm
  (norm [x]
    (ejml/norm x 2))
  (normalize [x]
    (scale x (/ 1.0 (norm x))))
  (dist [x ^DenseMatrix64F y]
    (norm (minus x y)))
  )


;;; Disable generated codox API documentation for some vars
(alter-meta! #'map-double-array assoc :no-doc true)
