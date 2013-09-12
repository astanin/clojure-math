(ns clojure.math.geometry.fit
  "Geometric fitting."
  (:use [clojure.math.geometry :only [plus minus scale dot]]
        clojure.math.internal.ejml))



(defn- dominant-direction
  [pts]
  (let [^DenseMatrix64F
        m (to-matrix pts)    ; M is an N x dim matrix
        mip (mult-inner m)   ; M' * M => dim x dim matrix
        epairs (eig mip)]
    (:eigenvector (last (sort-by :eigenvalue epairs)))))


(defn- average-vector
  [pts]
  (let [add-count (fn [[s n] x] [(plus s x) (+ 1 n)])
        z (scale (first pts) 0.0)
        [s n] (reduce add-count [z (long 0)] pts)]
    (scale s (/ 1.0 n))))


(defn- matrix1d-to-vector
  [d]
  (vec (.getData d)))


(defn fit-line
  "Fits a line to a sequence of points. Returns a map with keys :point and :direction."
  [pts]
  (let [p (average-vector pts)
        d (dominant-direction (map #(minus % p) pts))]
    {:point (vec p)
     :direction (matrix1d-to-vector d)}))


(defn fit-plane
  "Fits a plane to a sequence of points. Returns a map with
  keys :point (a point belonging to the plane), :normal (a vector
  normal to the plane), :basis (vectors of an orhonormal basis in the
  plane, and :offset (the signed distance `d` from the plane to the
  origin: given normal is [a b c], the plane equation is `ax + by + cz
  + d = 0`). The function can also be used to fit a line in 2D."
  [pts]
  (let [p (average-vector pts)
        dim (count p)
        pts' (map #(minus % p) pts)
        ut (->> (to-matrix pts') trans svd :u trans from-matrix)
        normal (last ut)    ; assuming a unit vector
        basis (take (- dim 1) ut)
        offset (let [op (minus p (scale p 0))]
                 (- (dot op normal)))]
    {:point p
     :normal normal
     :basis basis
     :offset offset}))
