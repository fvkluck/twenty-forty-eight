(ns twenty.main
  (:require [cljfx.api :as fx]
            [clojure.string :as string]))

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
  (->> moves
       (filter (fn [[k v]] (legal-move? board v)))
       (into {})))

(defn has-won? [board]
  (->> board
       (apply concat)
       (some #(= 2048 %))
       (boolean)))

(defn is-lost? [board]
  (->> board
       (legal-moves)
       (empty?)))

(defn handle-keystroke [game ch]
  (let [board (:board game)
        new-board (if-let [move-fn ((legal-moves board) ch)]
                    (->> board
                         move-fn
                         generate-block)
                    board)]
    (cond
      (has-won? new-board) (-> game
                               (assoc :won true)
                               (assoc :board new-board))
      (is-lost? new-board) (assoc game :board [[4 0 0 3]])
      :else (assoc game :board new-board))))

(def state
  (atom {:game {:board (generate-start-board)
                :won false}
         :showing true}))

(defn board->str [board]
  (->> board
       (map #(map (partial format "%7d") %))
       (map (partial string/join " "))
       (string/join "\n")))

(defn root [{:keys [showing game]}]
  {:fx/type :stage
   :showing showing
   :scene {:fx/type :scene
           :on-key-pressed {:event/type ::key-pressed}
           :root {:fx/type :v-box
                  :padding 50
                  :children [{:fx/type :text
                              :text (if (:won game)
                                      "Congratulations!"
                                      "Reach 2048 to beat this game!")}
                             {:fx/type :text
                              :font {:family "monospaced"}
                              :text (board->str (:board game))}
                             {:fx/type :button
                              :text "close"
                              :on-action (fn [_] (swap! state assoc :showing false))}]}}})

(defn map-event-handler [e]
  (case (:event/type e)
    ::key-pressed (swap! state update :game #(handle-keystroke % (-> (:fx/event  e) .getCode .getName)))))

(def renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc assoc :fx/type root)
    :opts {:fx.opt/map-event-handler map-event-handler}))

(fx/mount-renderer state renderer)

(defn -main [args]
  (fx/mount-renderer state renderer))
