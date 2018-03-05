(ns ulmus.keyboard
  "Helpers to create signals represeting the keyboard state."
  (:require 
    [cljs.core :as core]
    [ulmus.core :as signal]
    [cljs.core.async :as async])
  (:require-macros
    [cljs.core.async.macros :as async-mac]))

(declare keydown-events)
(declare keyup-events)
(declare key-events)
(declare arrows)

(defn- from-dom-event
  [status e]
  {:status status
   :keycode (.-keyCode e)
   :key (.-key e)})

(defn initialize!
  "Must be called before using the functions in ulmus/keyboard."
  []
  (def keydown-events 
    (signal/map 
      (partial from-dom-event :down)
      (signal/from-event! (.-body js/document) "keydown")))
  (def keyup-events 
    (signal/map
      (partial from-dom-event :up)
      (signal/from-event! (.-body js/document) "keyup")))
  (def key-events (signal/merge keydown-events keyup-events)))

(defn teardown!
  "Called after [[initialize!]] to clean up associated event handlers."
  []
  (signal/teardown-signal-from-event! keydown-events)
  (signal/teardown-signal-from-event! keyup-events))

(defn is-down?
  "Takes a keycode and returns a signal indicating whether or not the key is pressed."
  [keycode]
  (signal/drop-repeats
    (signal/map
      (fn [e] (= (:status e) :down))
      (signal/filter 
        (fn [e] (= (:keycode e) keycode))
        key-events))))

(defn presses
  "Takes a keycode and returns a signal that will receive every keydown event."
  [keycode]
  (signal/filter true? (is-down? keycode)))

(defn releases
  "Takes a keycode and returns a signal that will receive every keyup event."
  [keycode]
  (signal/filter false? (is-down? keycode)))

(defn enter "Returns a signal indicating if enter is down." [] (is-down? 13))

(defn space "Returns a signal indicating if space is down." [] (is-down? 32))

(defn ctrl "Returns a signal indicating if ctrl is down." [] (is-down? 17))

(defn shift "Returns a signal indicating if shift is down." [] (is-down? 16))

(defn alt "Returns a signal indicating if alt is down." [] (is-down? 18))

(defn arrows
  "Returns a signal of a map {:left? bool :up? bool :down? bool :right bool}"
  []
  (signal/map
    (fn [[left? up? right? down?]]
      {:left? left?
       :up? up?
       :right? right?
       :down? down?})
    (signal/latest (is-down? 37)
                   (is-down? 38)
                   (is-down? 39)
                   (is-down? 40))))


(defn keys-down 
  "Returns a signal of a set containing all currently pressed keycodes."
  [] 
  (signal/foldp 
    (fn [keyset e]
      (if (= (:status e) :up)
        (disj keyset (:keycode e))
        (conj keyset (:keycode e))))
    #{} key-events))
