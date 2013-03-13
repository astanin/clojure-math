(ns clojure.math.signal-test
  (:use clojure.test
        clojure.math.signal))


(deftest test-conv-doubles-same-as-numpy
  (testing "conv output for doubles is same like in NumPy"
    (let [input (double-array [1 2 3 4 5])
          kern3 [0.25 0.5 0.25]
          kern4 [0.25 0.5 0.5 0.25]]
      (testing ":full output, odd-size kernel"
        (is (= [0.25, 1.0, 2.0, 3.0, 4.0, 3.5, 1.25]
               (vec (conv input kern3)))))
      (testing ":full output, even-size kernel"
        (is (= [0.25, 1.0, 2.25, 3.75, 5.25, 5.25, 3.5, 1.25]
               (vec (conv input kern4)))))
      (testing ":same output, odd-size kernel"
        (is (= [1.0, 2.0, 3.0, 4.0, 3.5]
               (vec (conv input kern3 :same)))))
      (testing ":same output, even-size kernel"
        (is (= [1.0, 2.25, 3.75, 5.25, 5.25]
               (vec (conv input kern4 :same)))))
      (testing ":same output, odd-size kernel"
        (is (= [2.0, 3.0, 4.0]
               (vec (conv input kern3 :valid)))))
      (testing ":same output, even-size kernel"
        (is (= [3.75, 5.25]
               (vec (conv input kern4 :valid))))))))
