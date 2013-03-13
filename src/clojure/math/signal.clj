(ns clojure.math.signal)


(set! *warn-on-reflection* true)
(set! *unchecked-math* true)


(defprotocol HasConvolution
  "Discrete convolution of one-dimensional sequences.

  Parameters:

  xs      a finite (of size N) or an infinite seqable

  kernel  a seqable of finite size K (an array, a vector)

  output  an optional parameter; one of :full, :valid, :same

          :full (default)  calculate every point with an overlap (N+K-1);
          :same            return exactly max(N, K) points,
                           pad with zeros at edges;
          :valid           return only (N-K+1) inner points,
                           without zero padding at the edges."
  (conv [xs kernel] [xs kernel output]))


;; original implementation by @mikera http://stackoverflow.com/a/8256512/25450
;; changed:
;;   - argument order (to match the protocol),
;;   - output size (to match NumPy convention),
;;   - removed normalization of the kernel
(defn- convolve-doubles-full [^doubles data-array ^doubles kernel-array]
  (let [ks (count kernel-array)
        ds (count data-array)
        output (double-array (dec (+ ks ds)))]
    (dotimes [i (int ds)]
      (dotimes [j (int ks)]
        (let [offset (int (+ i j))]
          (aset output offset (+ (aget output offset)
                                 (* (aget data-array i)
                                    (aget kernel-array j)))))))
    output))


;; TODO: avoid copying arrays when output is not :full.
;; Java doesn't support referencing a segment of an array without
;; copying http://stackoverflow.com/q/1100371/25450 => so we need
;; to write three independent implementations
(defn- convolve-doubles-same
  [^doubles arr ^doubles kernel]
  (let [ksize (int (alength kernel))
        outsize (int (max (alength arr) ksize))
        full (doubles (convolve-doubles-full arr kernel))
        fullsize (int (alength full))
        offset (int (/ (- fullsize outsize) 2))]
    (java.util.Arrays/copyOfRange full offset (+ offset outsize))))


(defn- convolve-doubles-valid
  [^doubles arr ^doubles kernel]
  (let [ksize (int (alength kernel))
        asize (int (alength arr))
        outsize (int (+ 1 asize (* -1 ksize)))
        full (doubles (convolve-doubles-full arr kernel))
        offset (int (dec ksize))]
    (java.util.Arrays/copyOfRange full offset (+ offset outsize))))


(extend-protocol HasConvolution
  (Class/forName "[D")
  (conv
    ([arr kernel]
       (conv arr kernel :full))
    ([arr kernel mode]
           (let [kernel (if (instance? (Class/forName "[D") kernel)
                          kernel
                          (double-array kernel))]
             (condp = mode
               :full (convolve-doubles-full (doubles arr) (doubles kernel))
               :same (convolve-doubles-same (doubles arr) (doubles kernel))
               :valid (convolve-doubles-valid (doubles arr) (doubles kernel))))))
  clojure.lang.PersistentVector
  (conv
    ([xs kernel]
       (conv xs kernel :full))
    ([xs kernel mode]
       (vec (conv (double-array xs) (double-array kernel) mode)))))
