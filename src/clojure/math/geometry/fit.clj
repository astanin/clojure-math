(ns clojure.math.geometry.fit
  "Geometric fitting."
  (:require [clojure.math.geometry :as geom])
  (:use [clojure.math.geometry :only [plus minus scale dot]]
        clojure.math.internal.ejml)
  (:import [org.ejml.data DenseMatrix64F]))


(set! *unchecked-math* true)
(set! *warn-on-reflection* true)


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
  [^DenseMatrix64F d]
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
  origin: given normal is unit vector [a b c], the plane equation is
  `ax + by + cz + d = 0`).

  The function can also be used to fit a line in 2D."
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


(defn- **2
  [^double x]
  (* x x))


(defn fit-circle-2d
  "Fits a circle to a sequence of points in plane by mapping points on
  the Riemann sphere and fitting a plane in 3D. Returns a map with
  keys :center (a vector), :radius (circle radius), and :radius2
  (squared circle radius).

  A. Strandlie, J. Wroldsen, R. Frühwirth, B. Lillekjendlie, Particle
  tracks fitted on the Riemann sphere, Computer Physics
  Communications, Volume 131, Issues 1–2, 1 September 2000, Pages
  95-108, ISSN 0010-4655, http://dx.doi.org/10.1016/S0010-4655(00)00086-2."
  [pts]
  (assert (= 2 (count (first pts))) "points are not 2D")
  (assert (<= 3 (count pts)) "need at least 3 points")
  (let [;; polar coordinates (cosines and sines instead of angles)
        rs   (map geom/norm pts)
        coss (map (fn [[x y] r] (/ x r)) pts rs)
        sins (map (fn [[x y] r] (/ y r)) pts rs)
        ;; points on a Riemann sphere
        xs   (map (fn [r c] (/ (* r c) (+ 1 (**2 r)))) rs coss)
        ys   (map (fn [r s] (/ (* r s) (+ 1 (**2 r)))) rs sins)
        zs   (map (fn [r] (/ (* r r) (+ 1 (**2 r)))) rs)
        ;; plane of the circle on the sphere
        pl   (fit-plane (map vector xs ys zs))
        [n1 n2 n3] (:normal pl)
        c          (:offset pl)
        ;; parameters of the circle
        x0   (- (/ n1 (* 2 (+ c n3))))
        y0   (- (/ n2 (* 2 (+ c n3))))
        r2   (/ (+ (**2 n1) (**2 n2) (- (* 4 c (+ c n3))))
                (* 4 (**2 (+ c n3))))]
    {:center [x0 y0]
     :radius (Math/sqrt r2)
     :radius2 r2}))


(defn- to-plane-coords
  "Return 2D coordinates in the plane basis."
  [plane pt3d]
  (let [[e1 e2] (:basis plane)
        p (geom/minus pt3d (:point plane))]
    [(dot e1 p)
     (dot e2 p)]))


(defn- from-plane-coords
  "Return 3D coordinates of the point in the plane."
  [plane pt2d]
  (let [[u v] pt2d
        [e1 e2] (:basis plane)]
    (geom/plus
     (geom/plus (:point plane) (geom/scale e1 u))
     (geom/scale e2 v))))


(defn fit-circle-3d
  "Fits a plane to a sequence of points in 3D, and then fits a circle
  to their projections to the plane. Returns a map with keys :center
  (a 3D vector), :radius, :radius2, and :plane (a plane of the circle)."
  [pts]
  (assert (= 3 (count (first pts))) "points are not 3D")
  (assert (<= 3 (count pts)) "need at least 3 points")
  (let [base-plane (fit-plane pts)
        plane-pts  (map #(to-plane-coords base-plane %) pts)
        circle-2d  (fit-circle-2d plane-pts)]
    (-> circle-2d
     (update-in [:center] #(from-plane-coords base-plane %))
     (assoc :plane base-plane))))
