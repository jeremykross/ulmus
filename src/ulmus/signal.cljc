(ns ulmus.signal
  (:refer-clojure :exclude [map filter])
  (:require
    loom.attr
    [clojure.core :as c]
    [clojure.set :as set]
    [loom.graph :as loom]))

(defonce ^:dynamic graph (atom (loom/digraph)))
(defonce ^:dynamic originators (atom []))

(defn last-update
  "Returns the time at which the signal last updated."
  ([s-$] (last-update @graph s-$))
  ([g s-$]
   (first (loom.attr/attr g s-$ :value))))

(defn current-value
  "Returns the current value of the signal."
  ([s-$] (current-value @graph s-$))
  ([g s-$]
   (second (loom.attr/attr g s-$ :value))))

(defrecord Signal [kind tag proc]
  clojure.lang.IDeref
  (deref [this] (current-value this)))

(prefer-method print-method java.util.Map clojure.lang.IDeref)

(defn reset-graph! [] (reset! graph (loom/digraph)))

(defn- now [] (System/currentTimeMillis))

(defn- all-successors
  [g nodes]
  (let [successors (partial loom/successors g)]
    (loop [all #{}
           curr nodes]
      (let [direct-successors
            (apply set/union (c/map successors curr))]
        (if (empty? direct-successors)
          all
          (recur (set/union all direct-successors)
                 direct-successors))))))

(defn- add-originators
  [graph originators current-time]
  (loop [g graph
         o originators]
    (if (empty? o)
      g
      (let [{:keys [signal value]} (first originators)]
        (recur
          (loom.attr/add-attr g signal :value [current-time value])
          (rest originators))))))

(defn- handle-node
  [node graph current-time]
  (let [forward
        (fn [v]
          (loom.attr/add-attr graph node :value [current-time v]))

        incoming
        (loom/predecessors graph node)

        incoming-data
        (c/map #(loom.attr/attr graph % :value) incoming)

        incoming-times (c/map first incoming-data)

        incoming-values (c/map second incoming-data)

        rewire
        (fn [sig])]

    (or
      (condp = (:kind node)
        :standard
        ((:proc node) forward incoming-values)

        :switch
        (if (some? #(= % current-time) incoming-times)
          ((:proc node) rewire incoming-times))

        false)

      graph)))

(defn- propogate
  [graph originators current-time]
  (let [invalid-nodes
        (sort-by
          #(loom.attr/attr graph % :height)
          (all-successors graph (c/map :signal originators)))]

    (loop [g (add-originators graph originators current-time)
           invalid invalid-nodes]
      (let [n (first invalid)]
        (if (not n)
          g
          (recur
            (handle-node n g current-time)
            (rest invalid)))))))

(defn transact!
  [thunk]
  (binding [originators (atom [])]
    (thunk)
    (swap! graph propogate @originators (now))))

(defn >!
  [s-$ v]
  (swap! originators conj {:signal s-$ :value v}))

(defn close!
  [s-$])

(defn make-signal
  [kind tag proc & incoming]
  (let [heights
        (c/map #(loom.attr/attr @graph % :height) incoming)

        height
        (if (empty? heights)
          0
          (inc (apply max heights)))


        out-$
        (Signal. kind
                 tag
                 proc)]

    (swap! graph
           (fn [g]
             (let [incoming-values
                   (c/map current-value incoming)]
               (as-> g <>
                 (loom/add-nodes <> out-$)
                 (apply loom/add-edges <> (c/map (fn [from] [from out-$]) incoming))
                 (loom.attr/add-attr <> out-$ :height height)
                 (loom.attr/add-attr <> out-$ :value (if (every? #(not (nil? %)) incoming-values)
                                                       [(now) (proc identity incoming-values)]))))))

    out-$))

(defn signal
  ([] (signal nil))
  ([v]
   (make-signal :input nil (fn [forward _] (forward v)))))

   
(defn switch
  [proc s-$]
  (make-signal :switch nil (fn [forward [v]] (forward v)) s-$))

(defn map
  [proc s-$]
  (make-signal :standard
               :map
               (fn [forward [v]]
                 (forward (proc v)))
               s-$))

(defn filter
  [pred s-$]
  (make-signal :standard
               :filter
               (fn [forward [v]]
                 (when (pred v)
                   (forward v)))
                 s-$))
 
