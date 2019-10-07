(ns ulmus.core
  (:refer-clojure :exclude [count
                            delay
                            distinct
                            map
                            merge
                            filter
                            parition
                            reduce])
  #?(:cljs (:require-macros [ulmus.core :refer [def-transacting transact]]))
  (:require 
    [clojure.core :as c]
    [ulmus.signal :as signal]
    [ulmus.transaction :as transaction]))


#?(:clj
   (defmacro transact
     [& body]
     `(let [result# (do ~@body)]
        (ulmus.transaction/propogate!)
        result#)))

#?(:clj
   (defmacro def-transacting
     [& args]
     (let [defs (c/map (fn [[src dst]]
                         `(def ~src (transacting ~dst)))
                       (c/partition 2 args))]
       `(do ~@defs))))

(defn- transacting
  [f]
  (fn [& args]
    (transact
      (apply f args))))

(def-transacting
  >! transaction/>!
  subscribe! transaction/subscribe!
  input-signal signal/input
  constant-signal signal/constant
  switch signal/switch
  prev signal/prev
  map signal/map
  filter signal/filter
  reduce signal/reduce
  merge-simultaneous signal/merge-simultaneous
  merge signal/merge
  zip signal/zip
  sample-on signal/sample-on
  sample-when signal/sample-when
  distinct signal/distinct
  pickmap signal/pickmap
  pickzip signal/pickzip
  pickmerge signal/pickmerge)
