(ns ulmus.touch
  (:require 
    [cljs.core :as core]
    [ulmus.core :as signal]
    [ulmus.event :as evt]
    [cljs.core.async :as async])
  (:require-macros
    [cljs.core.async.macros :as async-mac]))

(declare touchmove-events)
(declare touchdown-events)
(declare touchup-events)

(defn initialize!
  []
  (def touchmove-events (evt/from-event! (.-body js/document) "touchmove"))
  (def touchstart-events (evt/from-event! (.-body js/document) "touchstart"))
  (def touchend-events (evt/from-event! (.-body js/document) "touchend")))

(defn teardown!
  []
  (evt/teardown-signal-from-event! touchmove-events)
  (evt/teardown-signal-from-event! touchstart-events)
  (evt/teardown-signal-from-event! touchend-events))

(defn- touches-from-native-event
  [e]
  (loop [i 0 touches []]
    (if (= i (.-length (.-touches e))) 
      touches
      (recur (inc i) (conj touches (.item (.-touches e) i))))))

(defn- touch->map
  [t]
  {:x (.-clientX t)
   :y (.-clientY t)
   :id (.-identifier t)})

(defn touches
  []
  (signal/map (fn [e] 
                (mapv touch->map (touches-from-native-event e)))
              (signal/merge
                touchmove-events
                touchstart-events
                touchend-events)))

(defn taps
  []
  (signal/map (fn [e]
                (let [touches (touches-from-native-event e)]
                  (touch->map (first touches)))) touchstart-events))
