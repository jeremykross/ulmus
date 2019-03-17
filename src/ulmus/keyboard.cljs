(ns ulmus.keyboard
  (:require ulmus.dom
            [ulmus.signal :as ulmus]))

(def up-$
  (ulmus.dom/listen! "keyup" js/document true))

(def down-$
  (ulmus.dom/listen! "keydown" js/document true))

(def press-$
  (ulmus.dom/listen! "keypress" js/document true))

(defn press
  [keycode]
  (ulmus/filter 
    (fn [e] (= (.-keyCode e) keycode))
    press-$))

