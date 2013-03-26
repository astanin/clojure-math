(ns clojure.math.signal-test
  (:use clojure.test
        clojure.math.signal))


(deftest test-conv-same-as-numpy
  (testing "conv output is the same like in NumPy:"
    (let [input-array (double-array [1 2 3 4 5])
          input-seq   (range 1 6)
          input-vec   (vec input-seq)
          kern3 [0.25 0.5 0.25]
          kern4 [0.25 0.5 0.5 0.25]
          kern-long [100000 10000 1000 100 10 1]]
      (doseq [input [input-array input-seq input-vec]]
        (let [class-name (str (class input))]
          (testing (str class-name ", :full output, odd-size kernel")
           (is (= [0.25, 1.0, 2.0, 3.0, 4.0, 3.5, 1.25]
                  (vec (conv input kern3)))))
          (testing (str class-name ", :full output, even-size kernel")
            (is (= [0.25, 1.0, 2.25, 3.75, 5.25, 5.25, 3.5, 1.25]
                   (vec (conv input kern4)))))
          (testing (str class-name ", :full output, long kernel")
            (is (= [100000, 210000, 321000, 432100, 543210, 54321, 5432, 543, 54, 5]
                   (mapv int (conv input kern-long)))))
          (testing (str class-name ", :same output, odd-size kernel")
            (is (= [1.0, 2.0, 3.0, 4.0, 3.5]
                   (vec (conv input kern3 :same)))))
          (testing (str class-name ", :same output, even-size kernel")
            (is (= [1.0, 2.25, 3.75, 5.25, 5.25]
                   (vec (conv input kern4 :same)))))
          (testing (str class-name ", :same output, long kernel")
            (is (= [321000, 432100, 543210, 54321, 5432, 543]
                   (mapv int (conv input kern-long :same)))))
          (testing (str class-name ", :same output, odd-size kernel")
            (is (= [2.0, 3.0, 4.0]
                   (vec (conv input kern3 :valid)))))
          (testing (str class-name ", :same output, even-size kernel")
            (is (= [3.75, 5.25]
                   (vec (conv input kern4 :valid)))))
          (testing (str class-name ", :same output, long kernel")
            (is (= [543210, 54321]
                   (mapv int (conv input kern-long :valid))))))))))
