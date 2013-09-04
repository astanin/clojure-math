(ns clojure.math.geometry.fit
  (:use [clojure.math.geometry :only [plus minus scale]])
  (:import [org.ejml.data DenseMatrix64F]
           [org.ejml.ops CommonOps]
           [org.ejml.alg.dense.decomposition.eig SymmetricQRAlgorithmDecomposition]))


(defn- to-matrix
  "Creates and EJML Matrix64F."
  ([seq-of-seqs]
     (if (instance? DenseMatrix64F seq-of-seqs)  ; safe to call (to-matrix (to-matrix ..))
       seq-of-seqs
       (let [^"[[D" arr2d (into-array (map double-array seq-of-seqs))]
        (DenseMatrix64F. arr2d))))
  ([num-rows num-cols]
     (DenseMatrix64F. num-rows num-cols)))


(defn- from-matrix
  "Converts an EJML Matrix64F to a Clojure vector or vectors."
  [^DenseMatrix64F m]
  (if (or (= 1 (.numRows m)) (= 1 (.numCols m)))
    (into [] (.getData m))  ; 1D matrix to a simple vector
    (mapv vec (partition (.numCols m) (.getData m))) ; 2D matrix to vector of vectors
    ))


(defn- eig!
  "Computes eigen-value decomposition of a symmetric matrix m,
  returns pairs of eigenvalue and eigenvector.
  Overwrites the matrix m."
  [^DenseMatrix64F m]
  (let [^SymmetricQRAlgorithmDecomposition
        sqr (SymmetricQRAlgorithmDecomposition. true)]
    (.decompose sqr m)
    (let [pairs (for [^int i (range (.numRows m))]
                  {:eigenvalue (.real (.getEigenvalue sqr i))
                   :eigenvector (.getEigenVector sqr i)})]
      pairs)))


(defn- det
  "Computes determinant of the matrix."
  [^DenseMatrix64F m]
  (CommonOps/det ))


(defn- dominant-direction
  [pts]
  (let [^DenseMatrix64F
        m (to-matrix pts)    ; M is an N x dim matrix
        dim (.numCols m)     ; dim is 3 for 3D points
        m3 (to-matrix dim dim)
        m3 (do (CommonOps/multInner m m3) m3) ; M' * M => dim x dim matrix
        epairs (eig! m3)]
    (:eigenvector (last (sort-by :eigenvalue epairs)))))


(defn- average-vector
  [pts]
  (let [add-count (fn [[s n] x] [(plus s x) (+ 1 n)])
        z (scale (first pts) 0.0)
        [s n] (reduce add-count [z (long 0)] pts)]
    (scale s (/ 1.0 n))))


(defn fit-line
  "Fits a line to a sequence of points. Returns a map with keys :point and :direction."
  [pts]
  (let [p (average-vector pts)
        d (dominant-direction (map #(minus % p) pts))]
    {:point (vec p)
     :direction (vec (.getData d))}))
