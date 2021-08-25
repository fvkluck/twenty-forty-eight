(ns twenty.main
  (:require [cljfx.api :as fx]
            [clojure.string :as string]
            [twenty.game :as tg]))

(defn nof-different-blocks [board1 board2]
  (let [v1 (apply concat board1)
        v2 (apply concat board2)]
    (reduce + (map (fn [x y] (if (= x y) 0 1)) v1 v2))))

(defn handle-keystroke [state ch]
  (case ch
    "W" (update state :game #(tg/make-move % :up-move))
    "S" (update state :game #(tg/make-move % :down-move))
    "A" (update state :game #(tg/make-move % :left-move))
    "D" (update state :game #(tg/make-move % :right-move))
    :else state))

(def state
  (atom {:game {:board (tg/generate-start-board)
                :state :playing}
         :showing true}))

(defn board->str [board]
  (->> board
       (map #(map (partial format "%7d") %))
       (map (partial string/join " "))
       (string/join "\n")))

(defn number->image [nr]
  {:fx/type :image-view
   :image {:url (str "file:resources/tiles/" nr ".jpg")
           :requested-width 50
           :preserve-ratio true}})

(defn game-field [{:keys [game]}]
    {:fx/type :v-box
     :padding 50
     :children [{:fx/type :text
                 :text (case (:state game)
                         :won "Congratulations!"
                         :playing "Reach 2048 to beat this game!"
                         :lost "I'm sorry for your loss")}
                {:fx/type :v-box
                 :children (for [row (:board game)]
                              {:fx/type :h-box
                               :children (map number->image row)})}]})

(defn root [{:keys [showing game]}]
  {:fx/type :stage
   :showing showing
   :scene {:fx/type :scene
           :on-key-pressed {:event/type ::key-pressed}
           :root {:fx/type :v-box
                  :padding 50
                  :children [{:fx/type game-field
                              :game game}
                             {:fx/type :h-box
                              :children [{:fx/type :button
                                          :text "restart"
                                          :on-action {:event/type ::restart-pressed}}
                                         {:fx/type :button
                                          :text "close"
                                          :on-action (fn [_] (swap! state assoc :showing false))}]}]}}})

(defn map-event-handler [e]
  (case (:event/type e)
    ::key-pressed (swap! state #(handle-keystroke % (-> (:fx/event e) .getCode .getName)))
    ::restart-pressed (swap! state update :game #(-> %
                                                     (assoc :board (tg/generate-start-board))
                                                     (assoc :state :playing)))))

(def renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc assoc :fx/type root)
    :opts {:fx.opt/map-event-handler map-event-handler}))

(fx/mount-renderer state renderer)

(defn -main [args]
  (fx/mount-renderer state renderer))
