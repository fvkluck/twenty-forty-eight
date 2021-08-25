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
           (rotate-ccw [[1 2 3]
                        [4 5 6]
                        [7 8 9]]))))

  (testing "rotate-cw"
    (is (= [[13 9 5 1]
            [14 10 6 2]
            [15 11 7 3]
            [16 12 8 4]]
           (rotate-cw [[1 2 3 4]
                       [5 6 7 8]
                       [9 10 11 12]
                       [13 14 15 16]])))
    (is (= [[7 4 1]
            [8 5 2]
            [9 6 3]]
           (rotate-cw [[1 2 3]
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
    (is (= [4 0 0 0] (squash-row [2 0 0 2]))))

  (testing "moves"
    (is (= [[4 8 0 0]
            [4 0 0 0]
            [8 8 0 0]
            [16 16 0 0]]
           (left-move [[2 2 4 4]
                       [2 0 0 2]
                       [4 0 4 8]
                       [8 8 8 8]])))
    (is (= [[0 0 4 8]
            [0 0 0 4]
            [0 0 8 8]
            [0 0 16 16]]
           (right-move [[2 2 4 4]
                        [2 0 0 2]
                        [4 0 4 8]
                        [8 8 8 8]])))
    (is (= [[4 2 8 4]
            [4 8 8 2]
            [8 0 0 16]
            [0 0 0 0]]
           (up-move [[2 2 4 4]
                     [2 0 0 2]
                     [4 0 4 8]
                     [8 8 8 8]])))
    (is (= [[0 0 0 0]
            [4 0 0 4]
            [4 2 8 2]
            [8 8 8 16]]
           (down-move [[2 2 4 4]
                       [2 0 0 2]
                       [4 0 4 8]
                       [8 8 8 8]]))))

  (testing "empty-tiles"
    (is (= [[0 0] [1 1]] (empty-tiles [[0 1] [2 0]]))))

  (testing "legal-moves"
    (is (= [] (legal-moves [[1 2 3 4]
                            [5 6 7 8]
                            [9 10 11 12]
                            [13 14 15 16]]))
        (= [left-move right-move] (legal-moves [[2 2 3 4]
                                                [5 6 7 8]
                                                [9 10 11 12]
                                                [13 14 15 16]]))))

  (testing "make-move"
    (are [move-fn move] (<= (nof-different-blocks
                              (move-fn [[2 2 4 4]
                                         [2 0 0 2]
                                         [4 0 4 8]
                                         [8 8 8 8]])
                              (:board (make-move {:board [[2 2 4 4]
                                                  [2 0 0 2]
                                                  [4 0 4 8]
                                                  [8 8 8 8]]
                                          :won false} move))) 1)
         left-move :left-move
         right-move :right-move
         up-move :up-move
         down-move :down-move)))
