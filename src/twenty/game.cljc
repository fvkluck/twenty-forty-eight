(ns twenty.game
  (:require [clojure.string :as string]))

(defn pad-zeroes [n row]
  "Add zeroes to the end of row, to achieve length n."
  (concat row (repeat (- n (count row)) 0)))

(defn squash-row [row]
  "Squash row towards index 0

  Two consecutive blocks of the same value get merged to a single block.
  "
  (->> row
       (reduce
         (fn [acc x]
           (if (zero? x)
             acc
             (if (and (:can-merge acc)
                      (= (last (:result acc)) x))
               (-> acc
                   (assoc :result (conj (pop (:result acc)) (* 2 x)))
                   (assoc :can-merge false))
               (-> acc
                   (update :result conj x)
                   (assoc :can-merge true)))))
         {:can-merge false
          :result []})
       (:result)
       (filter (comp not zero?))
       (pad-zeroes (count row))
       (vec)))

(defn rotate-ccw [board]
  "Rotate the board counter clock-wise"
  (->> board
       (apply map vector)
       (reverse)
       (vec)))

(def rotate-cw
  "Rotate the board clock-wise"
  (apply comp (repeat 3 rotate-ccw)))

(defn left-move [board]
  (vec (for [row board]
    (squash-row row))))

(defn up-move [board]
  (->> board
       (rotate-ccw)
       (left-move)
       (rotate-cw)))

(defn right-move [board]
  (->> board
       (rotate-ccw)
       (rotate-ccw)
       (left-move)
       (rotate-cw)
       (rotate-cw)))

(defn down-move [board]
  (->> board
       (rotate-cw)
       (left-move)
       (rotate-ccw)))

(defn empty-tiles [board]
  (let [n (count board)]
    (->> (for [i (range n)
               j (range n)]
           [i j])
         (map (fn [[i j]]
                [[i j] (get-in board [i j])]))
         (filter (comp zero? second))
         (map first))))

(defn random-block []
  (rand-nth [2 2 2 4]))

(defn generate-block [board]
  (let [target (->> board
                    (empty-tiles)
                    (rand-nth))]
    (assoc-in board target (random-block))))

(defn generate-start-board []
  (->> [[0 0 0 0]
        [0 0 0 0]
        [0 0 0 0]
        [0 0 0 0]]
       generate-block
       generate-block))

(defn legal-move? [board move-fn]
  (not (= (move-fn board) board)))

(defn legal-moves [board]
  (->> [up-move down-move left-move right-move]
       (filter (fn [move-fn] (legal-move? board move-fn)))
       (into [])))

(defn has-won? [board]
  (->> board
       (apply concat)
       (some #(= 2048 %))
       (boolean)))

(defn is-lost? [board]
  (->> board
       (legal-moves)
       (empty?)))

(defn make-move [game move]
  (let [moves {:up-move up-move
               :down-move down-move
               :left-move left-move
               :right-move right-move}
        board (:board game)
        move-fn (moves move)
        new-board (if (legal-move? board move-fn)
                    (->> board
                         move-fn
                         generate-block)
                    board)]
    (cond
      (has-won? new-board) (-> game
                               (assoc :state :won)
                               (assoc :board new-board))
      (is-lost? new-board) (-> game
                               (assoc :state :lost)
                               (assoc :board new-board))
      :else (assoc game :board new-board))))

