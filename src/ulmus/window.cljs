(ns ulmus.window
  (:require ulmus.dom
            [ulmus.signal :as ulmus]))

(def resize-events-$ (ulmus.dom/listen! "resize" js/window))

(defn- dimension [] [(.-innerWidth js/window) (.-innerHeight js/window)])

(def dimensions-$
  (ulmus/start-with!
    (dimension)
    (ulmus/map dimension resize-events-$)))

(def width-$ (ulmus/map first dimensions-$))

(def height-$ (ulmus/map second dimensions-$))
