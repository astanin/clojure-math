(ns clojure.math.geometry-fit-test
  (:use clojure.test
        clojure.math.geometry.fit))

(deftest test-fit-line
  (testing "two-point fit"
    (is (= {:point [0.5 0.0], :direction [1.0 0.0]}
           (fit-line [[0 0] [1 0]]))))
  (testing "two-point fit with an offset from the origin"
    (is (= {:point [1.5 -0.5], :direction [0.7071067811865475 -0.7071067811865475]}
           (fit-line [[0 1] [3 -2]]))))
  (testing "many-point fit with an outlier"
    (is (= {:point [3.0 1.125], :direction [1.0 0.0]}
           (fit-line [[0 1] [1 1] [2 1] [3 1] [4 1] [5 1] [6 1] [3.0 2]])))))
