(ns ulmus.time
  "Creates signals useful when dealing with time."
  (:refer-clojure :exclude [delay])
  (:require 
    [ulmus.core :as signal]
    [cljs.core.async :as async])
  (:require-macros
    [cljs.core.async.macros :as async-mac]))

(defn- now 
  []
  (js/Date.now))

(defn every
  "Returns a signal that returns the current universal epoch time every
  interval milliseconds."
  [interval]
  (signal/from-fn
    (fn [observer]
      (let [last-called-time (atom (now))]
        (js/setInterval 
          (fn []
            (let [dt (- (now) @last-called-time)]
              (reset! last-called-time (now))
              (signal/next! observer (/ dt 1000))))
          interval)))))

(defn fps
  "Returns a signal that fires `hz` times per second."
  [hz]
  (every (/ 1000 hz)))

(defn fps-when 
  "Returns a signal that fires `hz` times per second when `when-s` is truthy."
  [hz when-$]
  (signal/filter (fn [_] @when-$) (fps hz)))

(defn with-lifetime
  "Takes a signal and returns a new signal of `[total-time-signal-has-existed latest-value]`"
  [s-$]
  (let [last-time (atom (now))]
    (signal/foldp
      (fn [[lifetime _] v]
        (let [dt (- (now) @last-time)]
          (reset! last-time (now))
          [(+ lifetime dt) v]))
      [0 nil]
      s-$)))

(defn percentage-duration
  "Takes a time in millis `t` and a signal `s-$`.  Returns a signal that moves from 0-1 as the
  lifetime of s-$ approaches t."
  [t s-$]
  (signal/map
    (fn [[lifetime _]]
      (if (< lifetime t)
        (/ lifetime t)
        :elmalike/completed))
    (with-lifetime s-$)))

(defn timeout
  "Creates a signal that fires a signal value after timeout millis"
  [t]
  (let [s-$ (signal/signal)]
    (async-mac/go
      (async/<! (async/timeout t))
      (signal/next! s-$ :elmalike/completed))
    s-$))

(defn with-timestamp
  "Takes a signal and returns a new signal of `[current-epoch-time latest-value]`"
  [s-$]
  (signal/map (fn [v] [(now), v]) s-$))

(defn delay
  "Takes a time `t` in millis and a signal `s-$`.  Returns a new signal that
  reflects the value of s-$ delayed by t."
  [t s-$]
  (let [out-ch (async/chan (async/sliding-buffer signal/+buffer-size+))
        ch (signal/tap-signal s-$ (async/sliding-buffer signal/+buffer-size+))]
    (async-mac/go-loop 
      [v (async/<! ch)]
      (when (not (nil? v))
        (async-mac/go
          (async/<! (async/timeout t))
          (async/>! out-ch v))
        (recur (async/<! ch))))
    (signal/signal out-ch)))

(defn since
  "Takes a time in millis `t` and signal `s-$`. Returns a signal of true or false
  indicating if `t` has passed since the last value on `s-$`."
  [t s-$]
  (let [out-ch (async/chan (async/sliding-buffer signal/+buffer-size+))
        ch (signal/tap-signal s-$)]
    (async-mac/go-loop 
      [v (async/<! ch)]
      (async/>! ch true)
      (async/<! (async/timeout t))
      (async/>! ch false))
    (signal/signal out-ch)))

