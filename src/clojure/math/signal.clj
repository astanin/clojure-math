(ns clojure.math.signal)


(set! *warn-on-reflection* true)
(set! *unchecked-math* true)


(defprotocol HasConvolution
  (conv [xs kernel] [xs kernel output]
    "Discrete convolution of one-dimensional sequences.

Parameters:

xs      a finite (of size N) or an infinite seqable

kernel  a seqable of finite size K (an array, a vector)

output  an optional parameter; one of :full, :valid, :same

        :full (default)  calculate every point with an overlap (N+K-1);

        :same            return exactly max(N, K) points,
                         pad with zeros at edges;

        :valid           return only (N-K+1) inner points,
                         without zero padding at the edges."))


;; original implementation by @mikera http://stackoverflow.com/a/8256512/25450
;; changes:
;;   - argument order (to match the protocol),
;;   - output size (to match NumPy convention),
;;   - remove normalization of the kernel
;;   - calculate only an inner slice from offset to offset+outsize-1 (inclusive)
(defn- convolve-doubles
  [^doubles data-array ^doubles kernel-array offset outsize]
  (let [ks (count kernel-array)
        ds (count data-array)
        output (double-array outsize)]
    (dotimes [i (int ds)]
      (dotimes [j (int ks)]
        (let [outidx (int (- (+ i j) offset))]
          (if (< -1 outidx outsize)
            (aset output outidx
                  (+ (aget output outidx)
                     (* (aget data-array i)
                        (aget kernel-array j))))))))
    output))


(defn- convolve-doubles-full
  [^doubles arr ^doubles kernel]
  (let [asize (int (alength arr))
        ksize (int (alength kernel))
        outsize (int (+ asize ksize -1))]
   (convolve-doubles arr kernel (int 0) outsize)))


(defn- convolve-doubles-same
  [^doubles arr ^doubles kernel]
  (let [ksize (int (alength kernel))
        asize (int (alength arr))
        outsize (int (max asize ksize))
        fullsize (int (+ asize ksize -1))
        offset (int (/ (- fullsize outsize) 2))]
    (convolve-doubles arr kernel offset outsize)))


(defn- convolve-doubles-valid
  [^doubles arr ^doubles kernel]
  (let [ksize (int (alength kernel))
        asize (int (alength arr))]
    (if (< ksize asize)
      (let [outsize (int (+ asize (* -1 ksize) 1))
            offset (int (dec ksize))]
        (convolve-doubles arr kernel offset outsize))
      (convolve-doubles-valid kernel arr)  ; swap args if kernel is longer than arr
      )))


(defn- convolve-seqs
  [xs kernel]
  (lazy-seq
   (when (seq xs)
     (let [p (reduce + (map * (rseq kernel) xs))]
       (cons p (convolve-seqs (drop 1 xs) kernel))))))


(defn- convolve-seqs-full
  [xs kernel]
  (let [kernel (vec kernel)
        ksize (count kernel)
        zeropad (repeat (dec ksize) 0)
        xs' (lazy-cat zeropad xs)]
    (convolve-seqs xs' kernel)))


(defn- convolve-seqs-same
  [xs kernel]
  (let [ksize (count kernel)
        min-nsize (count (take ksize xs))
        small-kernel? (= ksize min-nsize)
        full (convolve-seqs-full xs kernel)
        {skip :skip same-seq :same} (if small-kernel?
                                      {:skip (int (/ (dec ksize) 2))
                                       :same xs}
                                      {:skip (int (/ (- (count full) ksize) 2))
                                       :same kernel})]
    (map (fn [r _] r) (drop skip full) same-seq)))


(defn- convolve-seqs-inner
  [xs kernel]
  (lazy-seq
   (when (seq (drop (dec (count kernel)) xs))
     (let [p (reduce + (map * (rseq kernel) xs))]
       (cons p (convolve-seqs-inner (drop 1 xs) kernel))))))


(defn- convolve-seqs-valid
  [xs kernel]
  (let [kernel (vec kernel)
        ps (convolve-seqs-inner xs kernel)]
    (if (seq ps)
      ps
      (convolve-seqs-inner kernel (vec xs))  ; swap args when kernel is longer than xs
      )))


(defn- as-double-array
  [coll]
  (if (instance? (Class/forName "[D") coll)
    coll
    (double-array coll)))


(extend-protocol HasConvolution
  (Class/forName "[D")
  (conv
    ([arr kernel]
       (convolve-doubles-full (doubles arr) (doubles (as-double-array kernel))))
    ([arr kernel mode]
           (let [kernel (as-double-array kernel)]
             (condp = mode
               :full (convolve-doubles-full (doubles arr) (doubles kernel))
               :same (convolve-doubles-same (doubles arr) (doubles kernel))
               :valid (convolve-doubles-valid (doubles arr) (doubles kernel))))))
  clojure.lang.IPersistentVector
  (conv
    ([xs kernel]
       (conv xs kernel :full))
    ([xs kernel mode]
       (vec (conv (double-array xs) (double-array kernel) mode))))
  clojure.lang.ISeq
  (conv
    ([xs kernel]
       (convolve-seqs-full xs kernel))
    ([xs kernel mode]
       (condp = mode
         :full (convolve-seqs-full xs kernel)
         :same (convolve-seqs-same xs kernel)
         :valid (convolve-seqs-valid xs kernel)))))

;;; FIFO for conj/peek/pop, sorted as a sequence
(deftype SortedQueue
  [aqueue sset]
  clojure.lang.Counted
  (count [this]
    (count (.sset this)))
  clojure.lang.IPersistentCollection
  (cons [this x]
    (SortedQueue. (conj (.aqueue this) x)
                  (conj (.sset this) x)))
  (empty [this]
    (SortedQueue. (empty (.aqueue this))
                  (empty (.sset this))))
  clojure.lang.IPersistentStack
  (pop [this]
    (let [aqueue (.aqueue this)
          sset (.sset this)
          fst (peek aqueue)]
      (SortedQueue. (pop aqueue) (disj sset fst))))
  (peek [this]
    (peek (.aqueue this)))
  clojure.lang.Seqable
  (seq [this]
    (seq (.sset this))))


(defn sorted-queue
  "Returns a new sorted queue with supplied values."
  [& vals]
  (let [aqueue (into clojure.lang.PersistentQueue/EMPTY vals)
        sset (apply sorted-set vals)]
    (SortedQueue. aqueue sset)))


(defmethod print-method SortedQueue
  [^SortedQueue q w]
  (print-method '<queue_ w)
  (print-method (seq (.aqueue q)) w)
  (print-method '|sorted_ w)
  (print-method (seq q) w)
  (print-method '> w))


;;; scipy.signal.medfilt outputs exactly n median values for n inputs.
;;;
;;; It appears its output with window (2*k+1) can be reproduced by
;;; taking all subsequences with at least (k+1) successive elements,
;;; and taking the (1+k)-th element from the end of every subsequence.
;;;
;;; def ranges(xs, n):
;;;     return [xs[max(0,i-(n//2)):i+(n//2)+1] for i in range(len(xs))]
;;;
;;; def like_medfilt(xs, n):
;;;     k = n//2
;;;     return map(lambda ss: sorted(ss)[-(k+1)], ranges(xs, n)
;;;
;;; The (k+1)-th element from the end for a subsequence of length L,
;;; is the same as (L-k)-th element from the beginning (1-indexed).

;; a variant of order-filter using a SortedQueue
(defn- order-filter-sq
  [xs n rank]
  (let [k (int (/ n 2))
        xs' (lazy-cat xs (repeat k nil))  ; pad with nils to have n outputs
        enqueue (fn [q x]
                  (if x
                    (if (>= (count q) n)
                      (conj (pop q) x)
                      (conj q x))  ; build-up a queue initially
                    (pop q)  ; don't enqueue trailing nils
                    ))
        subseqs (drop (inc k)  ; only subseqs with at least (k+1) elements
                      (reductions enqueue (sorted-queue) xs'))
        rankth  (fn [ss]
                  (nth (seq ss) (- (count ss) k 1)))]
    (map rankth subseqs)))


;; a variant of order-filter using an unsorted PersistentQueue
;;
;; | (count xs) |     n |-pq/-sq rel. time |
;; |------------+-------+------------------|
;; |    1000000 |     3 |             0.80 |
;; |    1000000 |    11 |             0.80 |
;; |    1000000 |   101 |             1.10 |
;; |    1000000 |  1001 |             1.64 |
;; |    1000000 | 10001 |             1.76 |
;;
(defn- order-filter-pq
  [xs n rank]
  (let [k (int (/ n 2))
        xs' (lazy-cat xs (repeat k nil))  ; pad with nils to have n outputs
        enqueue (fn [q x]
                  (if x
                    (if (>= (count q) n)
                      (conj (pop q) x)
                      (conj q x))  ; build-up a queue initially
                    (pop q)  ; don't enqueue trailing nils
                    ))
        subseqs (drop (inc k)  ; only subseqs with at least (k+1) elements
                      (reductions enqueue clojure.lang.PersistentQueue/EMPTY xs'))
        rankth  (fn [ss]
                  (nth (sort (seq ss)) (- (count ss) k 1)))]
    (map rankth subseqs)))


(defn medfilt
  "Apply a median filter to the input sequence xs with a window size ksize.
  ksize should be odd."
  [xs ksize]
  (assert (odd? ksize) "kernel size ksize should be odd.")
  (if (> ksize 40)
    (order-filter-sq xs ksize (int (/ ksize 2)))
    (order-filter-pq xs ksize (int (/ ksize 2)))))
