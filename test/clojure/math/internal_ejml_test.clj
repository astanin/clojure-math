(ns clojure.math.internal-ejml-test
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
