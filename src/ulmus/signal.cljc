(ns ulmus.signal
  (:refer-clojure :exclude [clone distinct merge map filter reduce zipmap])
  (:require
    [clojure.core :as c]
    [clojure.spec.alpha :as spec]))

#?(:cljs 
   (defrecord Signal [value proc inputs outputs]
     IDeref
     (-deref [_] @value))
   :default
   (defrecord Signal [value proc inputs outputs]
     clojure.lang.IDeref
     (deref [_] @value)))

#?(:cljs
   (extend-protocol IPrintWithWriter
     Signal
     (-pr-writer [sig writer _] (write-all writer 
       (str "<signal (current-value: " @(:value sig) " inputs: " (count (:inputs sig)) " outputs: " (count @(:outputs sig)) ") >"))))
   :default 
   (defmethod print-method Signal [sig ^java.io.Writer writer]
     (.write writer (str "<signal (current-value: " @(:value sig) " inputs: " (count (:inputs sig)) " outputs: " (count @(:outputs sig)) ") >"))))
  
(defn make-signal
  [init proc inputs]
  (let [s-$ (->Signal (atom init) proc inputs (atom []))]
    (doseq [input-signal inputs]
      (swap! (:outputs input-signal) conj s-$))
    s-$))

(defn signal? [x] (instance? Signal x))

(defn signal-propogate!
  [signal-$]
  (doseq [output @(:outputs signal-$)]
    ((:proc output) output @signal-$)))

(defn >!
  [signal-$ value]
  (reset! (:value signal-$) value)
  (signal-propogate! signal-$))


(defn signal-of
  [v]
  (make-signal v false []))

(defn signal [] (signal-of nil))

(defn merge
  [& signals]
  (make-signal (if (signal? (first signals)) @(first signals) nil)
               >!
               signals))
  
(defn zip
  [& signals]
  (let [current-value (fn [] (mapv deref signals))]
    (make-signal (current-value)
                 (fn [sig-$ _] (>! sig-$ (current-value)))
                 signals)))


(defn reduce
  [proc init s-$]
  (make-signal (if @s-$ (proc init @s-$) init)
               (fn [sig-$ v]
                 (>! sig-$ (proc @sig-$ v)))
               [s-$]))
               

(defn filter
  [pred s-$]
  (make-signal nil
               (fn [sig-$ v]
                 (when (pred v)
                   (>! sig-$ v)))
               [s-$]))

(defn map
  [proc & signals]
  (let [current-value (fn [] 
                        (when (some #(not (nil? (deref %))) signals)
                          (apply proc (mapv deref signals))))]
    (make-signal (current-value)
                 (fn [s-$ _] (>! s-$ (current-value)))
                 signals)))

(defn clone
  [s-$]
  (map identity s-$))

(defn sample-on
  [value-$ sample-$]
  (map (fn [_] @value-$) sample-$))

(defn distinct
  [s-$]
  (make-signal @s-$
               (fn [sig-$ v] (when (not= @sig-$ v) (>! sig-$ v)))
               [s-$]))

(defn subscribe!
  [s-$ proc]
  (when @s-$ (proc @s-$))
  (let [subscription (keyword (gensym))]
    (add-watch (:value s-$)
               subscription
               (fn [_ _ _ new-value]
                 (proc new-value)))
    subscription))

(defn unsubscribe!
  [s-$ subscription]
  (remove-watch (:value s-$) subscription))

(defn splice!
  [to-$ from-$]

  (js/console.log "Splice")
  (js/console.log (meta to-$))

  (when-let [m (meta to-$)]
    (when (:ulmus/splice-signal m)
      (unsubscribe! (:ulmus/splice-signal m)
                    (:ulmus/splice-subscription m))))

  (when @from-$
    (>! to-$ @from-$))

  (with-meta to-$ {:ulmus/splice-signal from-$
                   :ulmus/splice-subscription (subscribe! from-$ #(>! to-$ %))}))

(defn start-with!
  [v s-$]
  (>! s-$ v)
  s-$)

(defn pickmap
  [proc s-$]
  (let [map-$ (map proc s-$)
        out-$ (atom (signal))]
    (subscribe!
      map-$
      (fn [v-$]
        (when (signal? v-$)
          (reset! out-$
                  (splice! @out-$ v-$)))))
    @out-$))

(defn pickzip
  [proc s-$]
  (pickmap #(apply zip (c/map proc %)) s-$))

(defn pickmerge
  [proc s-$]
  (pickmap #(apply merge (c/map proc %)) s-$))


