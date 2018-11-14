(ns ulmus.signal
  (:require [clojure.spec.alpha :as spec]))

(defrecord Signal [value proc inputs outputs]
  clojure.lang.IDeref
  (deref [_] @value))

(defmethod print-method Signal [sig ^java.io.Writer writer]
  (.write writer (str "current-value: " @(:value sig) " inputs: " (count (:inputs sig)) " outputs: " (count @(:outputs sig)))))
  
(defn make-signal
  [init proc inputs]
  (let [s-$ (->Signal (atom init) proc inputs (atom []))]
    (doseq [input-signal inputs]
      (swap! (:outputs input-signal) conj s-$))
    s-$))

(defn signal-propogate!
  [signal-$]
  (doseq [output @(:outputs signal-$)]
    ((:proc output) output @signal-$)))

(defn signal-set!
  [signal-$ value]
  (reset! (:value signal-$) value)
  (signal-propogate! signal-$))

(defn signal-of
  [v]
  (make-signal v false []))

(defn signal-merge
  [& signals]
  (make-signal @(first signals)
               signal-set!
               signals))
  
(defn signal-zip
  [& signals]
  (let [current-value (fn [] (mapv deref signals))]
    (make-signal (current-value)
                 (fn [sig-$ _] (signal-set! sig-$ (current-value)))
                 signals)))


(defn signal-reduce
  [proc init s-$]
  (make-signal init
               (fn [sig-$ v]
                 (signal-set! sig-$ (proc @sig-$ v)))
               [s-$]))
               

(defn signal-filter
  [pred s-$]
  (make-signal nil
               (fn [sig-$ v]
                 (when (pred v)
                   (signal-set! sig-$ v)))
               [s-$]))


