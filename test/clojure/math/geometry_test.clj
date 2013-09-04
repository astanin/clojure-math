(ns clojure.math.geometry-test
  (:use clojure.test
        clojure.math.geometry
        clojure.math.internal.cmath3
        clojure.math.internal.ejml))


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
                 [(comp to-matrix vector) from-matrix "EJML row-vector"]
                 [(comp to-matrix #(map vector %)) from-matrix "EJML column-vector"]]]
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
