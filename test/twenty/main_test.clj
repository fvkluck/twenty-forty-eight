(ns twenty.main-test
  (:require [clojure.test :refer :all]
            [twenty.main :refer :all]))

(deftest main
  (testing "first-zero-index"
    (is (= 0 (first-zero-index [0 0 0 0])))
    (is (= 1 (first-zero-index [2 0 2 4])))
    (is (= 3 (first-zero-index [2 4 2 0])))
    (is (= nil (first-zero-index [2 4 2 4])))
    (is (= nil (first-zero-index []))))

  (testing "first-index"
    (is (= 0 (first-index zero? 0 [0 0 0 0])))
    (is (= 1 (first-index zero? 0 [2 0 2 4])))
    (is (= 1 (first-index zero? 1 [0 0 0 0])))
    (is (= nil (first-index zero? 0 [2 2 4 2])))
    (is (= nil (first-index zero? 0 []))))

  (testing "last-index"
    (is (= 0 (last-index zero? 0 [0 2 4 2])))
    (is (= 3 (last-index zero? 0 [0 0 0 0])))
    (is (= nil (last-index zero? 0 [2 4 2 4])))
    (is (= nil (last-index zero? 0 [])))
    (is (= 3 (last-index zero? 2 [2 4 0 0]))))

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

