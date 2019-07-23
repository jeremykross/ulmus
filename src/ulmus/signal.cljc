(ns ulmus.signal
  (:refer-clojure :exclude [map filter])
  (:require
    loom.attr
    [clojure.core :as c]
    [clojure.set :as set]
    [loom.graph :as loom]))

(defonce ^:dynamic graph (atom (loom/digraph)))
(defonce ^:dynamic originators (atom []))

(defrecord Signal [kind tag proc]
  clojure.lang.IDeref
  (deref [this] (loom.attr/attr @graph this :value)))

(prefer-method print-method java.util.Map clojure.lang.IDeref)

(defn- affected-from
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
  [graph originators]
  (loop [g graph
         o originators]
    (if (empty? o)
      g
      (let [{:keys [signal value]} (first originators)]
        (recur
          (loom.attr/add-attr g signal :value value)
          (rest originators))))))

(defn- handle-node
  [node graph]
  (let [forward
        (fn [v]
          (loom.attr/add-attr graph node :value v))

        store
        (fn [s-$]
          (loom.attr/attr graph s-$ :value))]

    (or
      (condp = (:kind node)
        :standard
        ((:proc node) store forward)

        false)

      graph)))


(defn- propogate
  [graph originators]
  (let [invalid-nodes
        (sort-by
          #(loom.attr/attr graph % :height)
          (affected-from graph (c/map :signal originators)))]

    (loop [g (add-originators graph originators)
           invalid invalid-nodes]
      (let [n (first invalid)]
        (if (not n)
          g
          (recur
            (handle-node n g)
            (rest invalid)))))))

(defn transact!
  [thunk]
  (binding [originators (atom [])]
    (thunk)
    (swap! graph propogate @originators)))

(defn >!
  [s-$ v]
  (swap! originators conj {:signal s-$ :value v}))

(defn make-signal
  [kind tag proc & incoming]
  (let [heights
        (c/map #(loom.attr/attr @graph % :height) incoming)
        height
        (inc (apply max (into [-1] heights)))
        out-$
        (Signal. kind
                 tag
                 proc)]
    (swap! graph
           (fn [g]
             (as-> g <>
               (loom/add-nodes <> out-$)
               (apply loom/add-edges <> (c/map (fn [from] [from out-$]) incoming))
               (loom.attr/add-attr <> out-$ :height height)
               (loom.attr/add-attr <> out-$ :value (proc #(loom.attr/attr <> % :value) identity)))))
    out-$))

(defn signal
  [v]
  (make-signal :input nil (fn [_ forward] (forward v))))

   
(defn switch
  [proc s-$]
  (make-signal :switch nil (fn [_ _]) s-$))

(defn map
  [proc s-$]
  (make-signal :standard
               :map
               (fn [store forward]
                 (forward (proc (store s-$))))
               s-$))

(defn filter
  [proc s-$]
  (make-signal :standard
               :filter
               (fn [store forward]
                 (let [v (store s-$)]
                   (when (proc v)
                     (forward v))))
               s-$))
 
; default, prev, switch

