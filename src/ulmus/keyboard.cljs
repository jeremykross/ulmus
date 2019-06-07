(ns ulmus.keyboard
  (:require ulmus.dom
            [ulmus.signal :as ulmus]))

(def up-$
  "A signal of keyup events."
  (ulmus.dom/listen! "keyup" js/document true))

(def down-$
  "A signal of keydown events."
  (ulmus.dom/listen! "keydown" js/document true))

(def press-$
  "A signal of keypress events."
  (ulmus.dom/listen! "keypress" js/document true))

(defn press
  "Accepts a keycode and returns a signal of matching keypress events"
  [keycode]
  (ulmus/filter 
    (fn [e] (= (.-keyCode e) keycode))
    press-$))

