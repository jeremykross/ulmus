(ns ulmus.mouse
  (:require ulmus.dom
            [ulmus.signal :as ulmus]))

(def move-events-$ 
  "A signal of mouse and touchmove events."
  (ulmus/merge
    (ulmus.dom/listen! "mousemove" (.-body js/document))
    (ulmus/map
      (fn [e] (aget (.-touches e) 0))
      (ulmus.dom/listen! "touchmove" (.-body js/document)))))

(defn- position [e] [(.-clientX e) (.-clientY e)])

(def position-$
  "A signal containing a 2 element vector of the current `[x, y]` position of the mouse."
  (ulmus/map position move-events-$))

(def mouseup-$
  "A signal of mouseup events."
  (ulmus.dom/listen! "mouseup" (.-body js/document)))

(def mousedown-$
  "A signal of mousedown events."
  (ulmus.dom/listen! "mousedown" (.-body js/document)))

(def down?-$
  "A boolean signal indicating whether or not the mouse is currently pressed."
  (ulmus/merge
    (ulmus/map (constantly true) mousedown-$)
    (ulmus/map (constantly false) mouseup-$)))
