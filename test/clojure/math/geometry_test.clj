(ns clojure.math.geometry-test
  (:require [clojure.math.internal.ejml :as ejml])
  (:use clojure.test
        clojure.math.geometry
        clojure.math.internal.cmath3))


(deftest test-scale-plus-minus-dot
  (testing "vector arithmetics:"
    ;; 2D and 3D cases
    (doseq [x [[1 2] [1 2 3]]]
      (testing (str (count x) "D")
        ;; various supported data types
        (doseq [[to-test-type from-test-type type-name]
                [[identity identity "vector"]
                 [double-array vec  "double-array"]
                 [to-vector from-vector "Commons Math vector"]
                 [(comp ejml/to-matrix vector) ejml/from-matrix "EJML row-vector"]
                 [(comp ejml/to-matrix #(map vector %)) ejml/from-matrix "EJML column-vector"]]]
          (let [xv (to-test-type x)]
            (testing type-name
              (testing "sum with itself is twice the original:"
                (is (= (from-test-type (scale xv 2))
                          (from-test-type (plus xv xv)))))
              (testing "difference with itself is zero:"
                (is (= (from-test-type (scale xv 0))
                       (from-test-type (minus xv xv)))))
              (testing "dot product with itself:"
                (is (= (double (apply + (map #(* % %) x)))
                       (double (dot xv xv))))))))))))


(deftest test-norm-normalize-dist
  (testing "L2-norm and distance"
    ;; 2D and 3D cases
    (doseq [[x norm-value] [[[3 4] 5]
                            [[2 3 6] 7]]]
      (doseq [[to-test-type type-name]
              [[identity "vector"]
               [double-array "double-array"]
               [to-vector "Commons Math vector"]
               [(comp ejml/to-matrix vector) "EJML row-vector"]
               [(comp ejml/to-matrix #(map vector %)) "EJML column-vector"]]]
        (testing (str "for " (count x) "D " type-name)
          (let [xv (to-test-type x)]
            (testing (str "norm value of " x " is " norm-value)
              (is (= (double norm-value)
                     (double (norm xv)))))
            (testing "normalized vector has norm = 1"
              (is (> 1e-15
                     (Math/abs (- 1.0 (norm (normalize xv)))))))
            (testing "distance from itself is zero"
              (is (> 1e-15
                     (dist xv xv))))))))))
