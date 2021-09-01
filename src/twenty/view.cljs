(ns twenty.view
  (:require [clojure.string :as string]
            [goog.dom :as dom]
            [goog.events :as events]
            [goog.string :as gstring]
            [goog.string.format]
            [twenty.game :as tg]
            [reagent.core :as reagent :refer [atom]]
            [reagent.dom :as rd]))

(defonce state (atom {:game {:board (tg/generate-start-board)
                             :state :playing}}))

(defn board->hiccup [board]
  (into [:div] (for [row board]
                 (into [:div] (for [x row]
                                [:img {:src (str "tiles/" x ".jpg")
                                       :width 50
                                       :height 50}])))))

(defn game-board [game]
  [:div (board->hiccup (:board @game))])

(defn app [state]
  (let [game (reagent/cursor state [:game])]
    [:div {:id "main"}
     [:h1 "hello happy world!"]
     [game-board game]]))

(defn handle-key [k]
  (case k
    "w" (swap! state update :game #(tg/make-move % :up-move))
    "s" (swap! state update :game #(tg/make-move % :down-move))
    "a" (swap! state update :game #(tg/make-move % :left-move))
    "d" (swap! state update :game #(tg/make-move % :right-move))))

(defn register-listeners []
  (events/listen (dom/getDocument)
                 ^string (.-KEYPRESS events/EventType)
                 #(handle-key (.-key %))))

(comment
  (register-listeners)
  (rd/render [app state] (.getElementById js/document "app")))

(defn main! []
  (println "Called main!")
  (register-listeners)
  (rd/render [app state] (.getElementById js/document "app")))
