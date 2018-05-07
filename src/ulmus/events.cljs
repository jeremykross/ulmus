(ns ulmus.event
  (:require [ulmus.core :as ulmus]))

(defn from-event!
  "Creates an elmalike signal from a DOM event.  Takes a dom element and the event name.
  Returns a signal that will receive the event object of each occurance of that event."
  [element event-name]
  (let [s-$ (ulmus/signal)
        handler #(ulmus/next! s-$ %)]
    (.addEventListener element event-name handler true)
    (with-meta s-$ {:event-name event-name
                    :event-handler handler})))

(defn teardown-signal-from-event!
  "A helper function to remove the event listener created by a call to [[from-event!]]."
  [s-$]
  (let [meta-data (meta s-$)
        event (:event-name meta-data)
        handler (:event-handler meta-data)]
    (when (and event handler)
      (.removeEventListener (.-body js/document) event handler))))

