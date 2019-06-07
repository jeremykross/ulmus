(ns ulmus.window
  (:require ulmus.dom
            [ulmus.signal :as ulmus]))

(defn- dimension [] [(.-innerWidth js/window) (.-innerHeight js/window)])

(defn- current-hash [] (.-hash (.-location js/window)))

(def resize-events-$
  "A signal of resize events."
  (ulmus.dom/listen! "resize" js/window))

(def hash-events-$
  "A signal of hashchange events."
  (ulmus.dom/listen! "hashchange" js/window))

(def dimensions-$
  "A signal containing a vector with the [w, h] dimensions of the window."
  (ulmus/start-with!
    (dimension)
    (ulmus/map dimension resize-events-$)))

(def width-$
  "A signal containing the window's current width."
  (ulmus/map first dimensions-$))

(def height-$
  "A signal containing the window's current width."
  (ulmus/map second dimensions-$))

(def hash-$
  "A signal containing the current hash."
  (ulmus/map current-hash hash-events-$))
