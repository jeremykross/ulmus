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
  "Creates a new signal."
  [init proc inputs]
  (let [s-$ (->Signal (atom init) proc (atom inputs) (atom []) (atom []) (atom false))]
    (doseq [input-signal inputs]
      (swap! (:outputs input-signal) conj s-$))
    s-$))

(defn signal?
  "Returns true if x is a signal, false otherwise"
  [x] (instance? Signal x))

(defn closed?
  "Returns true if s-$ is a signal and has been closed,
  false otherwise."
  [s-$] @(:closed? s-$))

(defn- signal-propogate!
  [signal-$]
  (doseq [output @(:outputs signal-$)]
    ((:proc output) output @signal-$)))

(defn >!
  "Puts a new value on the signal.  Because this function
  is imperatively mutative, it should be used sparingly
  outside of signal definitions."
  [signal-$ value]
  (reset! (:value signal-$) value)
  (signal-propogate! signal-$))


(defn signal-of
  "Creates a new signal containing the value `v`"
  [v]
  (make-signal v false []))

(defn signal
  "Creates a new signal without a value, the signal can only 
  ever be updated with `>!`."
  [] (signal-of nil))

(defn merge
  "Takes any number of signals and returns a signal containing the
  values emitted by any of them."
  [& signals]
  (make-signal (if (signal? (first signals)) @(first signals) nil)
               >!
               signals))
  
(defn zip
  "Takes any number of signals and returns a signal containing a 
  vector of each signals latest value."
  [& signals]
  (let [current-value (fn [] (mapv deref signals))]
    (make-signal (current-value)
                 (fn [sig-$ _] (>! sig-$ (current-value)))
                 signals)))


(defn reduce
  "Returns a signal whose value is the result of applying proc to the previous value
  of s-$ and it's current value.  `init` is the initial value of the signal."
  [proc init s-$]
  (make-signal (if @s-$ (proc init @s-$) init)
               (fn [sig-$ v]
                 (>! sig-$ (proc @sig-$ v)))
               [s-$]))
               

(defn filter
  "Returns a signal of values for which the value on s-$ applied to `pred` returns truthy."
  [pred s-$]
  (make-signal nil
               (fn [sig-$ v]
                 (when (pred v)
                   (>! sig-$ v)))
               [s-$]))

(defn map
  "Returns a signal whose value is `proc` applied to the values on the signals provided."
  [proc & signals]
  (let [current-value (fn [force?] 
                        (when (or force? (some #(not (nil? (deref %))) signals))
                          (apply proc (mapv deref signals))))]
    (make-signal (current-value false)
                 (fn [s-$ _] (>! s-$ (current-value true)))
                 signals)))

(defn clone
  "Creates a copy of a signal"
  [s-$]
  (map identity s-$))

(defn sample-on
  "Returns a signal that emits `value-$`'s current value when `sample` emits a value."
  [value-$ sample-$]
  (map (fn [_] @value-$) sample-$))

(defn sample-when
  "Returns a signal that emits new values from `value-$` when `sample-$` is assigned a truthy value."
  [value-$ sample-$]
  (make-signal (if @sample-$ @value-$)
              (fn [sig-$ v] (when @sample-$ (>! sig-$ v)))
              [value-$]))


(defn distinct
  "Returns a signal that will suppress consecutively `=` values on it's argument."
  [s-$]
  (make-signal @s-$
               (fn [sig-$ v] (when (not= @sig-$ v) (>! sig-$ v)))
               [s-$]))

(defn unsubscribe!
  "Removes a subscription from `s-$`."
  [s-$ subscription]
  (remove-watch (:value s-$) subscription)
  (remove-watch (:closed? s-$) subscription)
  (swap! (:subscriptions s-$)
         #(remove #{subscription} %)))

(defn subscribe!
  "Attaches subscriptions to `s-$`.  `update-proc` will get
  called with the value when s-$ emits.  `closed-proc` will
  get called when `s-$` is closed.  Should be used sparingly
  as an imperitive escape hatch for signal changes."
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
  "Subscribes a listener for the closing of a signal."
  [s-$ proc]
  (subscribe! s-$ identity proc))

(defn unsubscribe-all!
  "Removes all subscriptions from a signal."
  [s-$]
  (doseq [sub @(:subscriptions s-$)]
    (unsubscribe! s-$ sub)))

(defn splice!
  "Emits the values on `to-$` to `from-$`"
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
  "`s-$` is expected to emit a list of signals.
  Returns a signal of the values yielded by
  applying `proc` to the signal list emitted
  on `s-$`"
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
  "Creates a signal of the values yeilded by calling `zip`
  on the list of signals returned on `s-$` after applying `proc`."
  ([s-$] (pickzip identity s-$))
  ([proc s-$] (pickzip proc s-$ {}))
  ([proc s-$ metadata]
   (pickmap #(apply zip (c/map proc %)) s-$ metadata)))

(defn pickmerge
  "Creates a signal of the values yeilded by calling `merge`
  on the list of signals returned on `s-$` after applying `proc`."
  ([s-$] (pickmerge identity s-$))
  ([proc s-$] (pickmerge proc s-$ {}))
  ([proc s-$ metadata]
   (pickmap #(apply merge (c/map proc %)) s-$ metadata)))

(defn choose
  "Returns a signal containing the contents of the signal
  keyed by the value of `selection` in the `options` map."
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
  "Returns a signal of vectors containing every `n` elements
  emitted on `s-$`."
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
  "Returns a signal of vectors containing the last `n` elements
  emitted on `s-$`."
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
  "Takes a signal containing consecuative maps.  Returns a signal
  of the selected keys `[gained, lost]` between each two emissions."
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
  "Takes a signal of seqs.  Returns a signal that
  emits each element in the seq individually."
  (make-signal nil
               (fn [sig-$ v]
                 (if (seqable? v)
                   (doseq [element v] (>! sig-$ element))
                   (>! sig-$ v)))
               [s-$]))

#?(:cljs
   (defn- now [] (js/Date.now))
   :default 
   (defn- now [] (throw "implement me!")))

(defn throttle
  "Returns new values emitted on `s-$` no more than every `ms` millis."
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
     "Returns a signal of the last value emitted on `s-$` after `ms` millis
     have ellapsed since it receieved a value."
     [ms s-$]
     (let [timeout (atom nil)
           out-$ (signal)]
       (subscribe! s-$
                   (fn [v]
                     (when @timeout (js/clearTimeout @timeout))
                     (reset! timeout
                             (js/setTimeout (fn []
                                              (>! out-$ v)
                                              (reset! timeout nil)) ms))))
       out-$)))

#?(:cljs
   (defn delay
     "Returns each value emitted on `s-$` after a delay of `ms` millis."
     [ms s-$]
     (make-signal @s-$
                  (fn [sig-$ v]
                    (js/setTimeout #(>! sig-$ v) ms))
                  [s-$])))

(defn close!
  "Closes the signal.  Values will no longer be propogated
  to the signal and it can be garbage collected.  If the :transitive?
  keyed argument is provided, this function will also close any source
  signals for which `s-$` is the only target."
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

