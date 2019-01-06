(ns ulmus.mouse
  (:require ulmus.dom
            [ulmus.signal :as ulmus]))

(def move-events-$ 
  (ulmus/merge
    (ulmus.dom/listen! "mousemove" (.-body js/document))
    (ulmus/map
      (fn [e] (aget (.-touches e) 0))
      (ulmus.dom/listen! "touchmove" (.-body js/document)))))

(def position-$ (ulmus/map (fn [e]
                             [(.-clientX e)
                              (.-clientY e)]) move-events-$))


