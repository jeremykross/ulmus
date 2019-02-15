(ns ulmus.keyboard
  (:require ulmus.dom
            [ulmus.signal :as ulmus]))

(def up-$
  (ulmus.dom/listen! "keyup" js/document))

(def down-$
  (ulmus.dom/listen! "keydown" js/document))

(def press-$
  (ulmus.dom/listen! "keypress" js/document))

(defn press
  [keycode]
  (ulmus/filter 
    (fn [e] (= (.-keyCode e) keycode))
    press-$))

