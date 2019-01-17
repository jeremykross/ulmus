(ns ulmus.mouse
  (:require ulmus.dom
            [ulmus.signal :as ulmus]))

(def move-events-$ 
  (ulmus/merge
    (ulmus.dom/listen! "mousemove" (.-body js/document))
    (ulmus/map
      (fn [e] (aget (.-touches e) 0))
      (ulmus.dom/listen! "touchmove" (.-body js/document)))))

(defn position [e] [(.-clientX e) (.-clientY e)])

(def position-$ (ulmus/map position move-events-$))

(def mouseup-$ (ulmus.dom/listen! "mouseup" (.-body js/document)))

(def mousedown-$ (ulmus.dom/listen! "mousedown" (.-body js/document)))

(def down?-$ (ulmus/merge
               (ulmus/map (constantly true) mousedown-$)
               (ulmus/map (constantly false) mouseup-$)))
