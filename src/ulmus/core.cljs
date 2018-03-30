(ns ulmus.core
  "Core signal creation and mapipulation faculties."
  (:refer-clojure :exclude [merge map filter partition count clone reduce])
  (:require 
    [cljs.core :as c]
    [cljs.core.async :as async])
  (:require-macros
    [cljs.core.async.macros :as async-mac]))

(def ^:dynamic +buffer-size+ 1)

(defprotocol Observer
  "The functions here can be called on a signal to implicitly alter it's state.  These functions generally won't be called directly."
  (next! [this x])
  (error! [this err])
  (completed! [this]))

(defprotocol Observable
  "These functions here can be used to attach listeners to events occuring on signals.  These generally won't be called directly."
  (subscribe-next! [this f-next])
  (subscribe-error! [this f-error])
  (subscribe-completed! [this f-comleted])
  (subscribe! [this f-next f-error f-completed])
  (unsubscribe! [this subscription]))

(defrecord Signal 
  [ch mult latest completed?]
  Observer
  (next! [this x]
    (reset! latest x)
    (async-mac/go (async/>! ch x))
    this)
  (completed! [this]
    (reset! completed? true)
    (async/close! ch)
    this)
  (error! [this error]
    (next! this error)
    (completed! this)
    this)
  Observable
  (subscribe-next! [this f-next] (subscribe! this f-next identity identity))
  (subscribe-error! [this f-error] (subscribe! this identity f-error identity))
  (subscribe-completed! [this f-completed] (subscribe! this identity identity f-completed))
  (subscribe! [this f-next f-completed f-error]
    (when @this (f-next @this))
    (let [subscriber-ch (async/tap mult (async/chan))]
      (async-mac/go-loop 
        [x (async/<! subscriber-ch)]
        (if (or (nil? x) (= :unsubscribe x))
          (f-completed)
          (do
            (f-next x) 
            (recur (async/<! subscriber-ch)))))
      subscriber-ch))
  (unsubscribe! [this subscription]
    (async/untap mult subscription)
    (async-mac/go 
      (async/>! subscription :unsubscribe)))
  IDeref
  (-deref [this] @latest))

(defn signal
  "Generates a new Signal optionally taking a backing channel.  If
  no channel is specified, will create a channel with a sliding-buffer of size
  [[ulmus.core/+buffer-size+]] defaulting to 1."
  ([]
   (signal (async/chan (async/sliding-buffer +buffer-size+))))
  ([ch]
   (let [new-sig-$ (Signal. ch 
                          (async/mult ch) 
                          (atom nil) 
                          (atom false))]
     (subscribe-next! new-sig-$
              (fn [v] 
                (if (= v :elmalike/completed)
                  (completed! new-sig-$)
                  (reset! (:latest new-sig-$) v))))
     new-sig-$)))

(defn from-fn
  "A helper fn to create signals by applying `f` with the value of a fresh signal."
  [f]
  (let [s-$ (signal)]
    (f s-$)
    s-$))

(defn signal-of
  "Creates a signal containing a signal value `data`."
  [data]
  (let [s-$ (signal)]
    (next! s-$ data)
    s-$))

(defn is-completed?
  "Returns true if the signal is completed."
  [s-$]
  (deref (:completed? s-$)))

(defn from-event!
  "Creates an elmalike signal from a DOM event.  Takes a dom element and the event name.
  Returns a signal that will receive the event object of each occurance of that event."
  [element event-name]
  (let [s-$ (signal)
        handler #(next! s-$ %)]
    (.addEventListener element event-name handler true)
    (with-meta s-$ {:event-name event-name
                    :event-handler handler})))

(defn teardown-signal-from-event!
  "A helper function to remove the event listener created by a call to [[from-event!]]."
  [s-$]
  (let [meta-data (meta s-$)
        event (:event-name meta-data)
        handler (:event-handler meta-data)]
    (when (and event handler)
      (.removeEventListener (.-body js/document) event handler))))


(defn tap-signal
  "Takes a signal + optional buffer and transducer.  Returns a new channel
  that will recieve the successive values of that signal run through the 
  transducer."
  ([s-$]
   (tap-signal s-$ (async/sliding-buffer +buffer-size+)))
  ([s-$ buf] (tap-signal s-$ buf nil)) 
  ([s-$ buf xform]
   (async/tap (:mult s-$) (async/chan buf xform))))

(def signal->chan "Alias for [[tap-signal]]." tap-signal)

(defn merge 
  "Takes any number of signals and returns a new signal that takes the values of each of
  those provided."
  [& signals]
  (signal (async/merge (c/map tap-signal signals)
                       (async/sliding-buffer +buffer-size+))))

(defn map
  "Takes a func and any number of signals.  Returns a new signal that applies
  the function to each value of the signals applied as positional arguments."
  [f & s]
  (let [taps (c/map tap-signal s)
        sig (signal (async/map f taps))
        s-dereferenced (c/map deref s)
        present-value (if (not-any? nil? s-dereferenced) (apply f s-dereferenced))]
    (if present-value
      (next! sig present-value)
      sig)))

(defn clone
  "Creates a copy of the provided signal."
  [s-$]
  (map identity s-$))

(defn pipe!
  "Takes values from `in-$` and moves them to `out-$` optionally applying a modifier func."
  ([in-$ out-$] (pipe! in-$ identity out-$))
  ([in-$ modifier-fn out-$]

   (when (not (nil? @in-$))
     (next! out-$ (modifier-fn @in-$)))

   (let [subscription (subscribe-next! in-$
                                      (fn [in]
                                        (next!
                                          out-$ (modifier-fn in))))]
     (with-meta
       out-$
       {in-$ subscription}))))

(defn unpipe!
  "Stops piping values created with pipe!"
  [in-$ out-$]
  (let [subscription (get (meta out-$) in-$)]
    (when subscription
      (unsubscribe! in-$ subscription))
    out-$))

(defn foldp
  "fold-into-the-past - Takes a function and an init value - will apply the func
  passing the previous value returned as the first argument and the latest value
  of the signal."
  [f init s-$]
  (let [out-sig-$ (signal)
        ch (tap-signal s-$)]
    (async-mac/go-loop [state init]
      (next! out-sig-$ state)
      (let [v (async/<! ch)]
        (if (nil? v)
          (completed! out-sig-$)
          (recur (f state v)))))
    out-sig-$))

(def reduce "Alias for [[foldp]]." foldp)

(defn start-with!
  "Sets the first value of a given signal implicitly."
  [v s-$]
  (next! s-$ v))

(defn latest
  "Takes any number of signals, returns a new signal that will
  take the value of a vector containing the latest value received
  on each of the constituent signals."
  [& sigs]
  (if (zero? (c/count sigs))
    (signal-of [])
    (binding [+buffer-size+ (* (c/count sigs) 10)]
      (let [sigs (c/map (fn [s] (or s (signal))) sigs)
            with-index (fn [[index s]] (map (fn [v] [index v]) s))
            index-sigs (apply 
                         merge (c/map with-index 
                                      (map-indexed (fn [i s] [i s]) sigs)))]
        (foldp
          (fn [state [index v]]
            (assoc state index v))
          (mapv deref sigs)
          index-sigs)))))

(defn latest-keyed
  "Similar to [[latest]].  Takes an object of keywords to signals
  and returns a signal that will take the value of an object mapping
  the latest value of each constituent signal to its associated key."
  [sigs]
  (let [with-key (fn [[k s]] (map (fn [v] [k v]) s))
        keyed-sigs (c/map with-key sigs)]
    (foldp
      (fn [state [k v]]
        (assoc state k v))
      (into {}
            (c/map (fn [[k s]] [k @s]) sigs))
      (apply merge keyed-sigs))))

(defn sliding-slice
  "Takes an integer `n` and a signal.  Returns a signal that takes the
  value of a vector containing the last `n` elements taken by the originating
  signal."
  [n s-$]
  (foldp 
    (fn [slice v]
      (if (< (c/count slice) n)
        (conj slice v)
        (-> slice
          (conj v)
          (subvec 1))))
    [@s-$] s-$))

(defn count
  "Takes a signal and returns a signal with the count of elements received thereon."
  [s-$]
  (foldp (fn [cnt _] (inc cnt)) 0 s-$))

(defn filter
  "Take a signal and a predicate.  Returns a signal that recieves values from it's
  source where the predicate returns truthy."
  [pred s-$]
  (signal (tap-signal s-$ (async/sliding-buffer +buffer-size+) (c/filter pred))))

(defn drop-repeats
  "Take a signal, returns a signal that will only relay one value for repeated values." 
  [s-$]
  (signal (tap-signal s-$ (async/sliding-buffer +buffer-size+) (dedupe))))

(defn sample-on
  "Takes two signals, returns a signal that relays the value of `s-a-$` when `s-b-$` delivers
  a truthy value."
  [s-a-$ s-b-$]
  (map (fn [_] (if (not (nil? @s-a-$)) @s-a-$ false)) s-b-$))

(defn sample-between
  "Takes three signals - returns a signal that will relay values of `sampled-$` only after the 
  delivery of a truthy value on `start-$` and before the delivery of a truthy value
  on `stop-$`."
  [sampled-$ start-$ stop-$]
  (let [out-$ (signal (async/chan (async/sliding-buffer +buffer-size+)))
        start-ch (tap-signal start-$)
        stop-ch (tap-signal stop-$)]

    (async-mac/go
      (loop []
        (async/<! start-ch)
        (async/poll! stop-ch) ;Take anything off the stop-ch, we only want to stop on new vals.
        (let [pipe-$ (pipe! sampled-$ out-$)]
          (async/<! stop-ch)
          (unpipe! sampled-$ pipe-$))
        (recur)))

    out-$))

(defn flat-map
  "Takes a function and a signal.  The signal is expected to return another signal the values of which
  will then be relayed to the return signal."
  ([f s-$] (flat-map f identity s-$))
  ([f1 f2 s-$]
   (let [out-$ (signal (async/chan (async/sliding-buffer +buffer-size+)))
         ch (tap-signal s-$)]
     (async-mac/go-loop [in-$ nil out-$ out-$ s (if (not (nil? @s-$)) @s-$ (async/<! ch))]
       (when (not (nil? s))
         (let [cs-$ (f1 s)]
           (cond (and cs-$ in-$)
                 (do 
                   (unpipe! in-$ out-$)
                   (recur cs-$ (pipe! cs-$ f2 out-$) (async/<! ch)))
                 cs-$
                 (recur cs-$ (pipe! cs-$ f2 out-$) (async/<! ch))
                 :else
                 (recur in-$ out-$ (async/<! ch))))))
     out-$)))
         
(defn- select-between
  [selector-sig other-sigs]
  (flat-map
    (fn [a]
      (other-sigs a))
    selector-sig))

(defn- signalize
  ([x]
   (foldp (fn [known incoming]
            (let [incoming-ids (keys incoming)
                  new-known (c/merge known
                                   (zipmap incoming-ids
                                           (c/map (fn [k] (or (known k) (signal))) incoming-ids)))]

              (doseq [[k v] new-known]
                (next! v (incoming k)))

              new-known))
          {} x))
   ([x id-fn]
    (foldp (fn [known incoming]
             (let [incoming-ids (map id-fn incoming)
                   new-known (c/merge known
                                    (zipmap incoming-ids
                                            (c/map (fn [k] (or (known k) (signal))) incoming-ids)))]

               (doseq [[k v] new-known]
                 (next! v (some #(= k (id-fn %)) incoming)))

               new-known))
           {} x)))


