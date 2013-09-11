(ns clojure.math.internal.ejml
  "Internal interface to EJML library functions and types."
  (:import [org.ejml.data DenseMatrix64F]
           [org.ejml.ops CommonOps NormOps SingularOps]
           [org.ejml.alg.dense.decomposition.eig SwitchingEigenDecomposition]
           [org.ejml.alg.dense.decomposition.svd SafeSvd SvdImplicitQrDecompose]))


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


(defn diag
  "Constructs a diagonal matrix from a vector."
  [v]
  (CommonOps/diag (double-array v)))


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


(defn svd
  "Computes singular value decomposition of matrix M as M = U * S * V',
   where U and V are orthogonal, and S is diagonal. Returns a map with
   keys :u, :s (a vector of singular values), and :v. May return nil
   if decomposition fails.

   Keyword arguments:

   :compact     If false and M is m x n, the matrices U and V' are square m
                x m and n x n respectively.  If true and M is m x n,
                svd calculates only the first k columns of U, and the
                first k rows of V', where k = min(m, n).
                (default: false)

   :compute-uv  If false, svd calculates only a vector of singular
                values :s. If true, svd calculates U and V in
                addition to S. (default: true)

   :order       If false, singular values are not ordered.
                If true, singular values are given in decreasing order.
                (default: true)"
  [^DenseMatrix64F m
   & {:keys [compact compute-uv order]
      :or   {compact true compute-uv true order true}}]
  (let [^SafeSvd svd (SafeSvd.
                      (SvdImplicitQrDecompose.
                       compact compute-uv compute-uv true))
        sucess? (.decompose svd m)]
    (when sucess?
      (if compute-uv
        (let [[rows cols] (matrix-shape m)
              u-cols (if compact (min rows cols) rows)
              v-rows (if compact (min rows cols) cols)
              u (make-matrix rows u-cols)
              v (make-matrix v-rows cols)
              s (.getSingularValues svd)]
          (.getU svd u false)
          (.getV svd v true)
          (when order
            (SingularOps/descendingOrder u false s (min rows cols) v true))
          {:u u
           :s (vec s)
           :v v})
        ;; else only singular values
        (let [s (.getSingularValues svd)]
          (if order
            {:s (vec (sort-by - s))}      ; descending order
            {:s (vec s)}))))))


(defn- eig!
  "Computes eigen-value decomposition of a real matrix m.
  Returns a sequance of maps with keys :eigenvalue and :eigenvector.
  Overwrites the matrix m."
  [^DenseMatrix64F m]
  (let [[rows cols] (matrix-shape m)
        ^SwitchingEigenDecomposition sed (SwitchingEigenDecomposition. (* rows cols))]
    (.decompose sed m)
    (let [pairs (for [^int i (range (.numRows m))]
                  {:eigenvalue (.real (.getEigenvalue sed i))
                   :eigenvector (.getEigenVector sed i)})]
      pairs)))


(defn eig
  "Computes eigen-value decomposition of a real matrix m.
  Returns a sequence of maps with keys :eigenvalue and :eigenvector."
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


(defn pinv
  "Computes the Moore-Penrose pseudo-inverse of M: M^{+} = (M' M)^{-1} M'."
  [^DenseMatrix64F m]
  (let [[rows cols] (matrix-shape m)
        minv (make-matrix cols rows)]
    (CommonOps/pinv m minv)
    minv))
