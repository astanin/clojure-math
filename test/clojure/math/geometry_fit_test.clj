(ns clojure.math.geometry-fit-test
  (:use clojure.test
        [clojure.math.geometry :only [dot dist]]
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


(deftest test-fit-circle-2d
  (testing "three-point fit"
    (let [circ (fit-circle-2d [[-1 0] [0 1] [1.0 0]])]
      (is (= [0.0 0.0] (:center circ)))
      (is (= 1.0 (:radius circ)))))
  (testing "fittting with noise ~ 0.1 for {:center [1 2], :radius 2}"
    (let [[x0 y0 r0 noise] [1.0 2.0 2.0 0.1]
          pts (for [phi (range -90 90 12)]
                (let [a (/ (* Math/PI phi) 180)
                      c (Math/cos a)
                      s (Math/sin a)]
                  [(+ x0 (* r0 c) (rand noise))
                   (+ y0 (* r0 s) (rand noise))]))
          circ (fit-circle-2d pts)]
      (is (> (* 3 noise) (dist (:center circ) [x0 y0])))
      (is (> (* 3 noise) (- (:radius circ) r0))))))


(deftest test-fit-circle-3d
  (testing "three-point fit"
    (let [pts  [[-1 0 -2] [0 1 0] [1 0 2]]
          circ (fit-circle-3d pts)
          center (:center circ)
          radius (:radius circ)]
      (is (every? #(> 1e-10 (- radius (dist % center))) pts))))
  (testing "fittting with noise ~ 0.1 for {:center [1 2 3], :radius 2}"
    (let [[x0 y0 z0 r0 noise] [1.0 2.0 3.0 2.0 0.1]
          pts (for [phi (range -90 90 12)]
                (let [a (/ (* Math/PI phi) 180)
                      c (Math/cos a)
                      s (Math/sin a)]
                  [(+ x0 (* r0 c) (rand noise))
                   (+ y0 (* r0 s) (rand noise))
                   (+ z0 (rand noise))]))
          circ (fit-circle-3d pts)]
      (is (> (* 3 noise) (dist (:center circ) [x0 y0 z0])))
      (is (> (* 3 noise) (- (:radius circ) r0)))
      (is (> (* 3 noise) (min (dist (:normal (:plane circ)) [0 0 1])
                              (dist (:normal (:plane circ)) [0 0 -1])))))))
