(ns twenty.main
  (:require [cljfx.api :as fx]
            [clojure.string :as string]))

(def board
  [[2 0 2 0]
   [0 8 0 8]
   [2048 0 0 0]
   [0 2 2 0]])

(defn pad-zeroes [n row]
  "Add zeroes to the ned of row, to achieve length n."
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

(def moves
  {"W" up-move
   "S" down-move
   "A" left-move
   "D" right-move})

(defn empty-tiles [board]
  (let [n (count board)]
    (->> (for [i (range n)
               j (range n)]
           [i j])
         (map (fn [[i j]]
                [[i j] (get-in board [i j])]))
         (filter (comp zero? second))
         (map first))))

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
  (->> moves
       (filter (fn [[k v]] (legal-move? board v)))
       (into {})))

(defn has-won? [board]
  (->> board
       (apply concat)
       (some #(= 1024 %))
       (boolean)))

(defn handle-keystroke [board ch]
  (let [new-board (if-let [move-fn ((legal-moves board) ch)]
                    (->> board
                         move-fn
                         generate-block)
                    board)]
    (cond
      (has-won? new-board) [[\y \a \y \!]]
      (is-lost? new-board) [[\n \a \y \!]]
      :else new-board)))


(defn is-lost? [board]
  (->> board
       (legal-moves)
       (empty?)))

(defn random-block []
  (rand-nth [2 2 2 4]))

(defn -main [args]
  (println "hello"))

(def state
  (atom {:board (generate-start-board)
         :showing true}))

(defn board->str [board]
  (->> board
       (map #(map (partial format "%7d") %))
       (map (partial string/join " "))
       (string/join "\n")))

(defn root [{:keys [showing board]}]
  {:fx/type :stage
   :showing showing
   :scene {:fx/type :scene
           :on-key-pressed (fn [e] (swap! state update :board #(handle-keystroke % (-> e .getCode .getName))))
           :root {:fx/type :v-box
                  :padding 50
                  :children [{:fx/type :text
                              :font {:family "monospaced"}
                              :text (board->str board)}
                             {:fx/type :button
                              :text "close"
                              :on-action (fn [_] (swap! state assoc :showing false))}]}}})

(def renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc assoc :fx/type root)))

(fx/mount-renderer state renderer)
