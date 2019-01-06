(ns ulmus.dom
  (:require [ulmus.signal :as ulmus]))

(defn listen!
  [event element]
  (let [s-$ (ulmus/signal)
        listener (fn [e] (ulmus/>! s-$ e))]
    (.addEventListener element event listener)
    (with-meta
      s-$
      {:ulmus/listener listener})))
