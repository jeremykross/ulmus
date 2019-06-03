(ns ulmus.window
  (:require ulmus.dom
            [ulmus.signal :as ulmus]))

(defn- dimension [] [(.-innerWidth js/window) (.-innerHeight js/window)])

(defn- current-hash [] (.-hash (.-location js/window)))

(def resize-events-$ (ulmus.dom/listen! "resize" js/window))
(def hash-events-$ (ulmus.dom/listen! "hashchange" js/window))

(def dimensions-$
  (ulmus/start-with!
    (dimension)
    (ulmus/map dimension resize-events-$)))

(def width-$ (ulmus/map first dimensions-$))
(def height-$ (ulmus/map second dimensions-$))

(def hash-$ (ulmus/map current-hash hash-events-$))
