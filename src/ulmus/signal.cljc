(ns ulmus.signal
 (:refer-clojure :exclude [map]) 
 (:require [clojure.core :as c]
           [ulmus.transaction :as transaction]))

(defrecord Signal
  [kind tag proc height value incoming outgoing]
  clojure.lang.IDeref
  (deref [this] @(:value this)))

(prefer-method print-method clojure.lang.IDeref clojure.lang.IRecord)
(prefer-method print-method clojure.lang.IDeref clojure.lang.IPersistentMap)
(prefer-method print-method clojure.lang.IDeref java.util.Map)

(defn signal
  [kind tag proc incoming]
  (let [out-$
        (Signal. kind tag proc
                 (if (empty? incoming)
                   (atom 0)
                   (atom 
                     (inc
                       (apply
                         max
                         (c/map (comp deref :height) incoming)))))
                 (atom nil)
                 (atom incoming)
                 (atom []))]

  (doseq [i-$ incoming]
    (swap! (:outgoing i-$)
           conj out-$))

  (transaction/>! out-$ nil)

  out-$))

(defn constant
  [x]
  (signal :standard :constant (fn [f _] (f x)) []))

(defn map
  [proc & sigs]
  (signal :standard :map (fn [f vs] (f (apply proc vs))) sigs))
