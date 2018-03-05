# ulmus

Ulmus is a small library atop core.async designed to facilitate a functional-reactive style of programming.  It's inspired by Elm and RX, primarily.  Some of the API apes Elm although it makes no attempt to be complete or exclusive in doing so.  

Ulmus is basically a thin layer atop core.async.  It's designed to be easy to grok, and easily extensible.  

## Primer

The basic building block of Ulmus is termed a signal.  Behind the scenes it's represented by a core.async channel along with some additional data.  A signal is, conceptually, a value that can change over time.  Signals can be composed using an array of standard functional operators (defined in ulmus.core) or by applying transducers.  These operators all return new signals that can be further composed.  When the value on any of the signals update, the entire computation graph is re-run, keeping everything up to date.  The current value of a signal can be dereferenced just like an atom.

## Example
It's a bit contrived but lets create an example signal that returns true if the mouse is on the right half of the browser window and false otherwise.

```
(def right-side?-$ 
  (ulmus.core/map
    (fn [[window-width mouse-x]] (> mouse-x (/ window-width 2)))
    (ulmus.core/latest (ulmus.window/width) (ulmus.mouse/x))))
```

Now right-side?-$ represents the correct value whether the window is resized or the mouse moves.

If you want to do something further with that value (like maybe, set the state on a React compontent) you can use elmalike.core/subscribe-next! to have a callback called with the current value.

Alternatively you can derefernece (`@right-side?-$`) to get the value from anywhere else.


## License

Copyright © 2108 Jeremy Kross

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
