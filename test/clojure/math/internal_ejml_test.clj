(ns clojure.math.internal-ejml-test
  (require [clojure.math.geometry :as geom])
  (:use clojure.test
        clojure.math.internal.ejml))


(defn- approx=
  [x y]
  (> 1e-15 (Math/abs (- x y))))


(deftest test-norm-same-as-numpy
  (testing "matrix and vector norms are the same like in NumPy:"
    (let [m (to-matrix [[1 2] [3 4]])
          v (to-matrix (map vector [1 2 3]))]
      (doseq [[ord numpy-norm-value] [[1 6]
                                [2 5.4649857042190426]
                                [:inf 7]
                                [:fro 5.4772255750516612]]]
        (testing (str "matrix norm " ord)
          (is (approx= (double numpy-norm-value)
                       (norm m ord)))))
      (doseq [[ord numpy-norm-value] [[0 3]
                                [1 6]
                                [2 3.7416573867739413]
                                [:inf 3]]]
        (testing (str "vector norm " ord)
          (is (approx= (double numpy-norm-value)
                       (norm v ord))))))))


(deftest test-mult-outer
  (is (= [[5.0 11.0] [11.0 25.0]]
         (from-matrix (mult-outer (to-matrix [[1 2] [3 4]])))))
  (is (= [[5.0 11.0 17.0] [11.0 25.0 39.0] [17.0 39.0 61.0]]
         (from-matrix (mult-outer (to-matrix [[1 2] [3 4] [5 6]]))))))


(deftest test-trans
  (is (= [[1. 4.] [2. 5.] [3. 6.]]
         (from-matrix (trans (to-matrix [[1 2 3] [4 5 6]]))))))


(deftest test-pinv
  (is (> 1e-12
         (norm (geom/minus (to-matrix [[-23 -6 11] [-2 0 2] [19 6 -7]])
                           (geom/scale (pinv (to-matrix [[1 2 3] [4 5 6] [7 8 9]])) 36))))))
