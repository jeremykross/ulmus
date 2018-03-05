(ns ulmus.mouse
  "Helpers to create signals indicating the mouse state."
  (:require 
    [cljs.core :as core]
    [ulmus.core :as signal]
    [cljs.core.async :as async])
  (:require-macros
    [cljs.core.async.macros :as async-mac]))

(declare position-events)
(declare mousedown-events)
(declare mouseup-events)

(defn initialize!
  "Must be called before the functions is ulmus.mouse can be used."
  []
  (def position-events (signal/from-event! (.-body js/document) "mousemove"))
  (def mousedown-events (signal/from-event! (.-body js/document) "mousedown"))
  (def mouseup-events (signal/from-event! (.-body js/document) "mouseup")))

(defn teardown!
  "Cleans up the event handlers attached in [[ulmus.mouse/initialize!]]."
  []
  (signal/teardown-signal-from-event! position-events)
  (signal/teardown-signal-from-event! mousedown-events)
  (signal/teardown-signal-from-event! mouseup-events))

(defn position
  "Returns a signal of the two element vector indicating the current page-relative
  position of the mouse."
  []
  (->> (signal/map 
         (fn [e] (.preventDefault e) [(.-pageX e) (.-pageY e)]) position-events)
       (signal/start-with! [0 0])))

(defn x "Returns a signal of the current x position of the mouse." [] (signal/map first (position)))

(defn y "Returns a signal of the current y position of the mouse." [] (signal/map second (position)))

(defn is-down?
  "Returns a signal indicating if a mouse button is currently pressed."
  []
  (signal/map 
    #(= % :down)
    (signal/merge
      (signal/map (constantly :down) mousedown-events)
      (signal/map (constantly :up) mouseup-events))))

(defn clicks
  "Returns a signal of click events."
  []
  (signal/map (constantly :click) mousedown-events))
