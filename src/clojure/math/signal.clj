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
        asize (int (alength arr))
        outsize (int (+ asize (* -1 ksize) 1))
        offset (int (dec ksize))]
    (convolve-doubles arr kernel offset outsize)))


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
