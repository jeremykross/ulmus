(ns ulmus.dom
  (:require [ulmus.signal :as ulmus]))

(defn listen!
  "Takes a DOM event name and an HTML element.
  Installs a listener on the element for the given event
  and returns a signal of the events as they occur."
  ([event element] (listen! event element false))
  ([event element use-capture?]
  (let [s-$ (ulmus/signal)
        listener (fn [e] (ulmus/>! s-$ e))]
    (.addEventListener element event listener use-capture?)
    (with-meta
      s-$
      {:ulmus/listener listener}))))
