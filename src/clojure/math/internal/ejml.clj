(ns clojure.math.internal.ejml
  "Internal interface to EJML library functions and types."
  (:import [org.ejml.data DenseMatrix64F]
           [org.ejml.ops CommonOps NormOps]
           [org.ejml.alg.dense.decomposition.eig SymmetricQRAlgorithmDecomposition]))


(defn- alloc-DenseMatrix64F
  ([^DenseMatrix64F m]
     (DenseMatrix64F. (.getNumRows m) (.getNumCols m)))
  ([rows cols]
     (DenseMatrix64F. rows cols)))


(defn matrix-shape
  [^DenseMatrix64F m]
  [(.getNumRows m) (.getNumCols m)])


(defn matrix?
  "Returns true if M is not a one-dimensional row (column) vector."
  [^DenseMatrix64F m]
  (not (or (= 1 (.numRows m)) (= 1 (.numCols m)))))


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
  (if (not (matrix? m))
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


(defn add
  "Adds matrix B to matrix A."
  [^DenseMatrix64F a ^DenseMatrix64F b]
  (let [c ^DenseMatrix64F (alloc-DenseMatrix64F a)]
    (CommonOps/add a b c)
    c))


(defn sub
  "Subtracts matrix B from matrix A."
  [^DenseMatrix64F a ^DenseMatrix64F b]
  (let [c ^DenseMatrix64F (alloc-DenseMatrix64F a)]
    (CommonOps/sub a b c)
    c))


(defn scalar-mult
  "Multiplies matrix A by a scalar alpha."
  [^DenseMatrix64F a alpha]
  (let [c ^DenseMatrix64F (alloc-DenseMatrix64F a)]
    (CommonOps/scale alpha a c)
    c))


(defn mult
  "Computes matrix to matrix product.

       P = A * B

       P_{ij} = \\sum_{k=1:n} A_{ik} B_{kj}
  "
  [^DenseMatrix64F a ^DenseMatrix64F b]
  (let [[ar ac] (matrix-shape a)
        [br bc] (matrix-shape b)
        p (alloc-DenseMatrix64F ar bc)]
    (CommonOps/mult a b p)
    p))


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


(defn mult-outer
  "Computes the matrix multiplication outer product

       P = M * M'

       P_{ij} = \\sum_{k=1:m} M_{ik} M_{jk}
  "
  [^DenseMatrix64F m]
  (let [dim (.numRows m)           ; M is an dim x N matrix
        out (make-matrix dim dim)  ; M*M' is a dim x dim matrix
        ]
    (CommonOps/multOuter m out)
    out))


(defn element-mult
  "Computes element-by-element multiplication."
  [^DenseMatrix64F m1 ^DenseMatrix64F m2]
  (let [out ^DenseMatrix64F (alloc-DenseMatrix64F m1)]
    (CommonOps/elementMult m1 m2 out)
    out))


(defn element-div
  "Computes element-by-element division."
  [^DenseMatrix64F m1 ^DenseMatrix64F m2]
  (let [out ^DenseMatrix64F (alloc-DenseMatrix64F m1)]
    (CommonOps/elementDiv m1 m2 out)
    out))


(defn sum-elements
  "Computes a sum of all elements."
  [^DenseMatrix64F m]
  (CommonOps/elementSum m))


(defn min-element
  "Returns an element with the minimum value."
  [^DenseMatrix64F m]
  (CommonOps/elementMin m))


(defn max-element
  "Returns an element with the maximum value."
  [^DenseMatrix64F m]
  (CommonOps/elementMax m))


(defn norm
  "Computes matrix or vector norm.

  The following norms can be calculated (similar to NumPy):

  ====  =============================  ==========================
  ord   norm for matrices              norm for vectors
  ====  =============================  ==========================
  0     --                             number of non-zero values
  1     max column-sum of abs. values  sum of absolute values
  2     2-norm (largest sing. value)   Euclidean norm (default)
  :inf  max row-sum of abs. values     max of absolute values
  :fro  Frobenius norm (default)       --

  "
  ([^DenseMatrix64F m]
     (if (matrix? m)
       (NormOps/normF m)
       (NormOps/normP2 m)))
  ([^DenseMatrix64F m ord]
     (if (matrix? m)
       ;; for matrices
       (condp = ord
         ;; 0-norm not implemented
         1 (NormOps/normP1 m)
         2 (NormOps/normP2 m)
         :inf (NormOps/normPInf m)
         :fro (NormOps/normF m))
       ;; for vectors
       (condp = ord
         0 (->> (from-matrix m)
                (filter #(not= 0 %))
                (count))
         1 (NormOps/normP1 m)
         2 (NormOps/normP2 m)
         :inf (NormOps/normPInf m)
         ;; Frobenius norm not implemented
         ))))


(defn trans
  "Transposes matrix."
  [^DenseMatrix64F m]
  (let [[rows cols] (matrix-shape m)
        mt (alloc-DenseMatrix64F cols rows)]
    (CommonOps/transpose m mt)
    mt))
