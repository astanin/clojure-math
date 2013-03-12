(ns clojure.math.geometry
  (:import [org.apache.commons.math3.geometry.euclidean.oned Vector1D]
           [org.apache.commons.math3.geometry.euclidean.twod Vector2D]
           [org.apache.commons.math3.geometry.euclidean.threed Vector3D]))


(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

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
              ~r))
       2 (let [n (gensym "n")
               a (gensym "a")
               b (gensym "b")
               r (gensym "r")
               i (gensym "i")
               fun (gensym "fun")]
           `(let [~(with-meta a {:tag 'doubles}) ~(first arrs)
                  ~(with-meta b {:tag 'doubles}) ~(second arrs)
                  ~fun ~f
                  ~n (alength ~a)
                  ~(with-meta r {:tag 'doubles}) (double-array ~n)]
              (dotimes [~i (int ~n)]
                (aset ~r ~i (~fun (aget ~a ~i) (aget ~b ~i))))
              ~r)))))


(extend-type (Class/forName "[D")
  EuclideanVector
  (plus [x y] (map-double-array + x y))
  (minus [x y] (map-double-array - x y))
  (scale [x ^double alpha] (map-double-array #(* alpha %) x))
  (dot [x y] (reduce + (map-double-array * x y)))
  (norm [x] (Math/sqrt (reduce + (map-double-array #(* % %) x))))
  (normalize [x] (let [l (norm x)] (map-double-array #(/ % l) x)))
  HasCross
  (cross [x y] (cross (vec x) (vec y))))


(defprotocol Vectorizable
  "Data types convertible to Commons Math's Euclidean vectors."
  (to-vector [coll] "Converts a sequence to a Common Math's vector."))


(defmacro derive-seqable
  "Derives a new class from base-class, and implements Seqable by
  using .toArray method of the parent."
  [base-class & init-args]
  `(proxy [~base-class clojure.lang.Seqable] [~@init-args]
     (seq [] (let [~(with-meta 'this {:tag `~base-class}) ~'this]
               (seq (.toArray ~'this))))))


(defn make-vector
  "Creates an Euclidean vector."
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


(extend-type Vector2D
  EuclideanVector
  (plus [x ^Vector2D y] (.add x y))
  (minus [x ^Vector2D y] (.subtract x y))
  (scale [x ^double alpha] (.scalarMultiply x alpha))
  (dot [x ^Vector2D y] (.dotProduct x y))
  (norm [x] (.getNorm x))
  (normalize [x] (.normalize x))
  HasCross
  (cross [[x1 x2] [y1 y2]] (- (* x1 y2) (* x2 y1))))


(extend-type Vector3D
  EuclideanVector
  (plus [x ^Vector3D y] (.add x y))
  (minus [x ^Vector3D y] (.subtract x y))
  (scale [x ^double alpha] (.scalarMultiply x alpha))
  (dot [x ^Vector3D y] (.dotProduct x y))
  (norm [x] (.getNorm x))
  (normalize [x] (.normalize x))
  HasCross
  (cross [x ^Vector3D y] (Vector3D/crossProduct x y)))
