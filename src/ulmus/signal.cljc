(ns ulmus.signal
 (:refer-clojure :exclude [count
                           delay
                           distinct
                           map
                           merge
                           filter
                           partition
                           reduce])
 (:require [clojure.core :as c]
           [ulmus.transaction :as transaction]))

(:clj
  (defmacro
    rec
    [x & body]
    `(let [~x (forward)
           s-$# (do ~@body)]
       (reset! (:height ~x) (inc @(:height s-$#)))
       (transaction/add-edge! s-$# ~x)
       ~x)))

(defrecord Signal
  [kind tag proc height value incoming outgoing]
  clojure.lang.IDeref
  (deref [this] @(:value this)))

(prefer-method
  print-method
  clojure.lang.IDeref clojure.lang.IRecord)

(prefer-method
  print-method
  clojure.lang.IDeref clojure.lang.IPersistentMap)

(prefer-method
  print-method
  clojure.lang.IDeref java.util.Map)

(defn signal
  [kind tag proc incoming]
  (let [out-$
        (Signal.
          kind tag proc
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

(defn input
  ([] (input nil))
  ([v]
   (let [out-$
         (signal :standard :input (fn [_ _]) [])]
     (when v (transaction/>! out-$ v))
     out-$)))

(defn constant
  [x]
  (signal :standard :constant (fn [f _] (f x)) []))

(defn forward
  ([] (forward nil))
  ([s-$]
   (signal :standard :forward (fn [f [v]] (f v)) (if s-$ [s-$] []))))

(defn switch
  [s-$]
  (let [switch-$ (signal :switch nil (fn [rewire [v-$]] (rewire v-$)) [s-$])
        fwd-$ (forward switch-$)]
    fwd-$))

(defn prev
  [s-$]
  (let [p-$ (signal :prev s-$ nil [])]
    (transaction/prev! p-$ s-$)
    p-$))

(defn map
  [proc & sigs]
  (signal :standard :map (fn [f vs] (f (apply proc vs))) sigs))

(defn filter
  [pred s-$]
  (signal :standard
          :filter
          (fn [f [v]]
            (when (pred v)
              (f v)))
          [s-$]))

(defn reduce
  [proc init s-$]
  (rec x (signal :standard
                 :reduce
                 (fn [f [acc v]]
                   (f (proc (or acc init) v)))
                 [(prev x)
                  s-$])))

(defn merge-simultaneous
  ([simultaneous & sigs]
   (signal :standard
           :merge
           (fn [f values]
             (let [present-values (remove nil? values)]
               (if (= 1 (c/count present-values))
                 (f (first present-values))
                 (f (simultaneous values)))))
           sigs)))

(def merge (partial merge-simultaneous first))

(defn zip
  [& signals]
  (signal :standard
          :zip
          (fn [f values]
            (f (into [] values)))
          signals))

(defn sample-on
  [trigger-$ value-$]
  (signal :standard
          :sample-on
          (fn [f [t v]]
            (when t (f (or v @value-$))))
          [trigger-$ value-$]))

(defn sample-when
  [trigger-$ value-$]
  (signal :standard
          :sample-when
          (fn [f [t v]]
            (when (or t @trigger-$)
              (f (or v @value-$))))
          [trigger-$ value-$]))

(defn distinct
  [s-$]
  (signal :standard
          :distinct
          (fn [f [v pv]]
            (when (not= v pv) (f v)))
          [s-$ (prev s-$)]))

(defn pickmap
  [proc s-$]
  (map proc (switch s-$)))

(defn pickzip
  [proc s-$]
  (pickmap #(apply zip (c/map proc %)) s-$))

(defn pickmerge
  [proc s-$]
  (pickmap #(apply merge (c/map proc %)) s-$))
