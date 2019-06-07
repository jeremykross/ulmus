# Ulmus

Ulmus is a library for doing Functional Reactive Programming (FRP) in Clojure and Clojurescript.

### Usage

`[ulmus "0.2.3"]` in `project.clj`

or

`{:deps {ulmus {:mvn/version "0.2.3"}}}` in `deps.edn`

### Introduction

Ulmus is a library designed to facilitate a functional-reactive style of programming. It's primarily inspired by Elm and RX. Some of the API apes Elm although it makes no attempt to be complete or exclusive in doing so.

Signals are the basic building blocks of ulmus.  Conceptually, a signal is a datatype which holds a particular value.  Signals can be composed with standard functional tooling (eg. map, filter, reduce), producing new, dependent, signals.  Changing a signal's value causes all of it's dependent signals to update as well.

So for the sake of illustration, we might define a signal representing the current position of the mouse.

```clojure
(defn mouse-pos
  []
  (let [sig-$ (ulmus/signal)]
    (.addEventListener
      js/body
      "mousemove"
      (fn [e]
        (ulmus/>! sig-$ [(.-clientX e) (.-clientY e)])))
    sig-$))

(def position-$ (mouse-pos))
```

We now have a signal, `position-$` which represents the current mouse position.  Dereferencing the signal will return it's current value.

```clojure
@position
```

might return [640 480] for instance. 

Just like static data, signals can be composed, yielding new signals.  So,

```clojure
(def mouse-x-$ (ulmus/map first position-$))
```

Signals can model functionally interactions that would otherwise require slipping into imperative coding.  To better understand how these ideas might be applied to user interface see [Recurrent](https://github.com/jeremykross/recurrent).

### Examples

Various examples can be found at https://jeremykross.github.io/recurrent-examples.

### API Docs

[![cljdoc badge](https://cljdoc.org/badge/ulmus)](https://cljdoc.org/d/ulmus/ulmus/CURRENT)


### Status

Ulmus is fairly well-tread at this point, but still beta quality.  It shouldn't be relied on yet for anything mission critical.

### Todo

* Spec

### License

Copyright 2019 Jeremy Kross

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
