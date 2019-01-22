(ns ulmus.keyboard
  (:require ulmus.dom
            [ulmus.signal :as ulmus]))

(def up-events-$
  (ulmus.dom/listen! "keyup" js/document))
