(ns clojure.math.geometry-fit-test
  (:use clojure.test
        [clojure.math.geometry :only [dot]]
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


(defn- plane-eq-residual
  [plane point]
  (+ (:offset plane) (dot point (:normal plane))))


(deftest test-fit-plane
  (testing "three-point fit in 3D"
    (let [pts [[0 0 0] [3 0 0] [0 3 0]]
          p (fit-plane pts)]
      (is (= [1.0 1.0 0.0] (:point p)))
      (is (or (= [0.0 0.0 1.0] (:normal p))
              (= [0.0 0.0 -1.0] (:normal p))))
      (is (every? #(= 0.0 (last %)) (:basis p)))
      (is (> 1e-10 (apply dot (:basis p))))
      (is (> 1e-10 (apply max (map #(plane-eq-residual p %) pts))))))
  (testing "five-point fit in 3D with an outlier"
    (let [pts [[0 0 0] [4 0 0] [0 4 0] [4 4 0] [2 2 1.25]]
          p (fit-plane pts)]
      (is (= [2.0 2.0 0.25] (:point p)))
      (is (or (= [0.0 0.0 1.0] (:normal p))
              (= [0.0 0.0 -1.0] (:normal p))))
      (is (every? #(= 0.0 (last %)) (:basis p)))
      (is (> 1e-10 (apply dot (:basis p))))
      (is (> (+ 1e-10 0.25) (apply max (map #(plane-eq-residual p %) pts)))))))
