(ns ulmus.signal
  (:refer-clojure :exclude [clone delay distinct merge map filter flatten partition reduce zipmap])
  (:require
    clojure.set
    [clojure.core :as c]
    [clojure.spec.alpha :as spec]))

#?(:cljs 
   (defrecord Signal [value proc inputs outputs subscriptions closed?]
     IDeref
     (-deref [_] @value))
   :default
   (defrecord Signal [value proc inputs outputs subscriptions closed?]
     clojure.lang.IDeref
     (deref [_] @value)))

#?(:cljs
   (extend-protocol IPrintWithWriter
     Signal
     (-pr-writer [sig writer _] (write-all writer 
       (str "<signal (current-value: " @(:value sig) " inputs: " (count @(:inputs sig)) " outputs: " (count @(:outputs sig)) ") >"))))
   :default 
   (defmethod print-method Signal [sig ^java.io.Writer writer]
     (.write writer (str "<signal (current-value: " @(:value sig) " inputs: " (count @(:inputs sig)) " outputs: " (count @(:outputs sig)) ") >"))))
  
(defn make-signal
  [init proc inputs]
  (let [s-$ (->Signal (atom init) proc (atom inputs) (atom []) (atom []) (atom false))]
    (doseq [input-signal inputs]
      (swap! (:outputs input-signal) conj s-$))
    s-$))

(defn signal? [x] (instance? Signal x))

(defn closed? [s-$] @(:closed? s-$))

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
  (let [current-value (fn [force?] 
                        (when (or force? (some #(not (nil? (deref %))) signals))
                          (apply proc (mapv deref signals))))]
    (make-signal (current-value false)
                 (fn [s-$ _] (>! s-$ (current-value true)))
                 signals)))

(defn clone
  [s-$]
  (map identity s-$))

(defn sample-on
  [value-$ sample-$]
  (map (fn [_] @value-$) sample-$))

(defn sample-when
  [value-$ sample-$]
  (make-signal (if @sample-$ @value-$)
              (fn [sig-$ v] (when @sample-$ (>! sig-$ v)))
              [value-$]))


(defn distinct
  [s-$]
  (make-signal @s-$
               (fn [sig-$ v] (when (not= @sig-$ v) (>! sig-$ v)))
               [s-$]))

(defn unsubscribe!
  [s-$ subscription]
  (remove-watch (:value s-$) subscription)
  (remove-watch (:closed? s-$) subscription)
  (swap! (:subscriptions s-$)
         #(remove #{subscription} %)))

(defn subscribe!
  ([s-$ update-proc] (subscribe! s-$ update-proc identity))
  ([s-$ update-proc closed-proc]
   (when (and
           @s-$
           (not (= (:replay? (meta s-$)) false)))
     (update-proc @s-$))
   (let [subscription (keyword (gensym))]
     (add-watch (:value s-$)
                subscription
                (fn [_ _ _ new-value]
                  (update-proc new-value)))
     (add-watch (:closed? s-$)
                subscription
                (fn [subscription]
                  (unsubscribe! s-$ subscription)
                  (closed-proc)))
     (swap! (:subscriptions s-$)
            conj subscription)
     subscription)))

(defn subscribe-closed!
  [s-$ proc]
  (subscribe! s-$ identity proc))

(defn unsubscribe-all!
  [s-$]
  (doseq [sub @(:subscriptions s-$)]
    (unsubscribe! s-$ sub)))

(defn splice!
  [to-$ from-$]

  (when-let [m (meta to-$)]
    (when (:ulmus/splice-signal m)
      (unsubscribe! (:ulmus/splice-signal m)
                    (:ulmus/splice-subscription m))))

  ;(when @from-$
  ;  (>! to-$ @from-$))

  (with-meta to-$ {:ulmus/splice-signal from-$
                   :ulmus/splice-subscription (subscribe! from-$ #(>! to-$ %))}))

(defn start-with!
  [v s-$]
  (>! s-$ v)
  s-$)

(defn pickmap
  ([proc s-$] (pickmap proc s-$ {}))
  ([proc s-$ metadata]
   (let [map-$ (map proc s-$)
         out-$ (atom (signal))]

     (subscribe!
       map-$
       (fn [v-$]
         (when (signal? v-$)
           (reset! out-$
                   (splice! @out-$ (with-meta v-$ metadata))))))
     @out-$)))

(defn pickzip
  ([s-$] (pickzip identity s-$))
  ([proc s-$] (pickzip proc s-$ {}))
  ([proc s-$ metadata]
   (pickmap #(apply zip (c/map proc %)) s-$ metadata)))

(defn pickmerge
  ([s-$] (pickmerge identity s-$))
  ([proc s-$] (pickmerge proc s-$ {}))
  ([proc s-$ metadata]
   (pickmap #(apply merge (c/map proc %)) s-$ metadata)))

(defn choose
  [selection-$ options]
  (let [out-$ (atom (signal))]
    (when-let [selected-option (options @selection-$)]
      (reset! out-$
              (splice! @out-$ selected-option)))
    (subscribe!
      selection-$
      (fn [selection]
        (when-let [selected-option (options selection)]
          (reset! out-$
                  (splice! @out-$ selected-option)))))
    @out-$))

(defn partition
  [n s-$]
  (let [buffer (atom [])]
    (make-signal nil
                 (fn [sig-$ v]
                   (swap! buffer conj v)
                   (when (= (count @buffer) n)
                     (>! sig-$ @buffer)
                     (reset! buffer [])))
                 [s-$])))

(defn slice
  [n s-$]
  (let [buffer (atom 
                 (concat
                   (repeat (dec n) nil)
                   [@s-$]))]
    (make-signal @buffer
                 (fn [sig-$ v]
                   (swap! buffer (fn [buf] (-> (drop 1 buf)
                                               (concat [v]))))
                   (>! sig-$ @buffer))
                 [s-$])))

(defn changed-keys
  [s-$]
  (map
    (fn [[prev curr]]
      (let [curr-keys (into #{} (keys curr))
            prev-keys (into #{} (keys prev))
            gained (clojure.set/difference curr-keys prev-keys)
            lost (clojure.set/difference prev-keys curr-keys)]
        [(select-keys curr gained)
         (select-keys prev lost)]))
    (slice 2 s-$)))

(defn flatten
  [s-$]
  (make-signal nil
               (fn [sig-$ v]
                 (if (seqable? v)
                   (doseq [element v] (>! sig-$ element))
                   (>! sig-$ v)))
               [s-$]))

#?(:cljs
   (defn now [] (js/Date.now))
   :default 
   (defn now [] (throw "implement me!")))

(defn throttle
  [s-$ ms]
  (let [last-put (atom (now))
        out-$ (signal)]
    (subscribe! s-$
                (fn [v]
                  (when (> (- @last-put (now)) ms)
                    (>! out-$ v)
                    (reset! last-put (now)))))
    out-$))

#?(:cljs
   (defn debounce
     [ms s-$]
     (let [timeout (atom nil)
           out-$ (signal)]
       (subscribe! s-$
                   (fn [v]
                     (js/console.log v)
                     (when @timeout (js/clearTimeout @timeout))
                     (reset! timeout
                             (js/setTimeout (fn []
                                              (>! out-$ v)
                                              (reset! timeout nil)) ms))))
       out-$)))

#?(:cljs
   (defn delay
     [ms s-$]
     (make-signal @s-$
                  (fn [sig-$ v]
                    (js/setTimeout #(>! sig-$ v) ms))
                  [s-$])))

(defn close!
  [s-$ & opts]
  (let [options (apply hash-map opts)]

    (doseq [input-$ @(:inputs s-$)]
      (swap! (:outputs input-$)
             #(remove #{s-$} %))
      (when (and
              (:transitive? options)
              (empty? @(:outputs input-$)))
        (apply close! input-$ opts)))

    (doseq [output-$ @(:outputs s-$)]
      (swap! (:inputs output-$)
             #(remove #{s-$} %)))

    (reset! (:closed? s-$) true)))

