(ns ulmus.time
  (:refer-clojure :exclude [delay])
  (:require 
    [ulmus.signal :as ulmus]))

(defn delay
  [ms s-$]
  (ulmus/make-signal
    @s-$
    (fn [sig-$ v]
      (js/setTimeout
        #(ulmus/>! sig-$ v)
        ms))
    [s-$]))

