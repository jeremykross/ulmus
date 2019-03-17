(ns ulmus.dom
  (:require [ulmus.signal :as ulmus]))

(defn listen!
  ([event element] (listen! event element false))
  ([event element use-capture?]
  (let [s-$ (ulmus/signal)
        listener (fn [e] (ulmus/>! s-$ e))]
    (.addEventListener element event listener use-capture?)
    (with-meta
      s-$
      {:ulmus/listener listener}))))
