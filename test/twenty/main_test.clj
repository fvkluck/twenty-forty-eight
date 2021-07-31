(ns twenty.main-test
  (:require [clojure.test :refer :all]
            [twenty.main :refer :all]))

(deftest main
  (testing "rotate-ccw"
    (is (= [[4 8 12 16]
            [3 7 11 15]
            [2 6 10 14]
            [1 5 9  13]]
           (rotate-ccw [[1 2 3 4]
                        [5 6 7 8]
                        [9 10 11 12]
                        [13 14 15 16]])))
    (is (= [[3 6 9]
            [2 5 8]
            [1 4 7]]
           (rotate-ccwd [[1 2 3]
                         [4 5 6]
                         [7 8 9]]))))

  (testing "pad-zeroes"
    (is (= [1 2 3 0 0 0] (pad-zeroes 6 [1 2 3])))
    (is (= [1 0 2 0 3 0] (pad-zeroes 6 [1 0 2 0 3])))
    (is (= [1 2 3] (pad-zeroes 3 [1 2 3])))
    (is (= [1 2 3] (pad-zeroes 0 [1 2 3]))))

  (testing "squash-row"
    (is (= [4 0 0 0] (squash-row [2 2 0 0])))
    (is (= [4 4 0 0] (squash-row [2 2 2 2])))
    (is (= [4 0 0 0] (squash-row [2 2 0 0])))
    (is (= [4 0 0 0] (squash-row [2 0 0 2])))))

