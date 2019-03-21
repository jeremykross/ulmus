(ns ulmus.time
  (:refer-clojure :exclude [delay])
  (:require 
    [ulmus.signal :as ulmus]))

(defn frame
  [s-$]
  (ulmus/make-signal
    @s-$
    (fn [sig-$ v]
      (js/requestAnimationFrame #(ulmus/>! sig-$ v)))
    [s-$]))

(defn delay
  [ms s-$]
  (ulmus/make-signal
    @s-$
    (fn [sig-$ v]
      (js/setTimeout
        #(ulmus/>! sig-$ v)
        ms))
    [s-$]))

