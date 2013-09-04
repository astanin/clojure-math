(ns clojure.math.internal.ejml
  "Internal interface to EJML library functions and types."
  (:import [org.ejml.data DenseMatrix64F]
           [org.ejml.ops CommonOps]
           [org.ejml.alg.dense.decomposition.eig SymmetricQRAlgorithmDecomposition]))


(defn make-matrix
  "Creates a new EJML Matrix64F."
  [num-rows num-cols]
  (DenseMatrix64F. num-rows num-cols))


(defn to-matrix
  "Converts a sequences of sequences to a new EJML Matrix64F."
  ([seq-of-seqs]
     (if (instance? DenseMatrix64F seq-of-seqs)  ; safe to call (to-matrix (to-matrix ..))
       seq-of-seqs
       (let [^"[[D" arr2d (into-array (map double-array seq-of-seqs))]
        (DenseMatrix64F. arr2d)))))


(defn from-matrix
  "Converts an EJML Matrix64F to a Clojure vector or vectors."
  [^DenseMatrix64F m]
  (if (or (= 1 (.numRows m)) (= 1 (.numCols m)))
    (into [] (.getData m))  ; 1D matrix to a simple vector
    (mapv vec (partition (.numCols m) (.getData m))) ; 2D matrix to vector of vectors
    ))


(defn eig!
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


(defn eig
  "Computes eigen-value decomposition of a symmetric matrix m,
  returns pairs of eigenvalue and eigenvector."
  [^DenseMatrix64F m]
  (eig! (.copy m)))


(defn det
  "Computes determinant of the matrix."
  [^DenseMatrix64F m]
  (CommonOps/det m))


(defn mult-inner
  "Computes the matrix multiplication inner product

       P = M' * M

       P_{ij} = \\sum_{k=1:n} M_{ki} M_{kj}
  "
  [^DenseMatrix64F m]
  (let [dim (.numCols m)           ; M is an N x dim matrix
        out (make-matrix dim dim)  ; M'*M is a dim x dim matrix
        ]
    (CommonOps/multInner m out)
    out))
