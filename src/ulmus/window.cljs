(ns ulmus.window
  "Helpers to create signals dealing with the size of the browser window."
  (:require 
    [cljs.core :as core]
    [ulmus.core :as signal]
    [ulmus.event :as evt]
    [cljs.core.async :as async])
  (:require-macros
    [cljs.core.async.macros :as async-mac]))

(declare resize-events)

(defn initialize!
  "Must be called before funcs in ulmus.window can be used."
  []
  (def resize-events (evt/from-event! js/window "resize")))

(defn teardown!
  "Called to cleanup event handlers attached in [[ulmus.window/initialize!]]."
  []
  (evt/teardown-signal-from-event! resize-events))

(defn dimensions
  "Returns a signal of a vector containing the current dimension of the window in px."
  []
  (->> resize-events
       (signal/map 
         (fn [e] [(.-innerWidth js/window) (.-innerHeight js/window)]))
       (signal/start-with!
         [(.-innerWidth js/window) (.-innerHeight js/window)])))

(defn width "Returns a signal of the current window width." [] (signal/map first (dimensions)))

(defn height "Returns a signal of the current window height." [] (signal/map second (dimensions)))



