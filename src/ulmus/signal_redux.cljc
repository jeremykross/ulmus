(ns ulmus.signal
  (:refer-clojure
    :exclude [count
              delay
              distinct
              map
              merge
              filter
              flatten
              partition
              reduce])
  (:require
    loom.attr
    [clojure.core :as c]
    [clojure.set :as set]
    [loom.graph :as loom]))

(defonce ^:dynamic graph (atom (loom/digraph)))
(defonce ^:dynamic originators (atom []))

#?(:clj
   (defmacro rec
     [x & body]
     `(let [~x (fwd)
            sig-$# (do ~@body)]

        (swap! graph
               (fn [g#]
                 (-> g#
                     (loom.attr/add-attr ~x :height (inc (height sig-$#)))
                     (loom.graph/add-edges [sig-$# ~x]))))

        ~x)))

#?(:clj
   (defmacro transact
     [& body]
     `(transact! #(do ~@body))))

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

(defn height
  ([s-$] (height @graph s-$))
  ([g s-$]
   (loom.attr/attr g s-$ :height)))

(defrecord Signal [kind tag proc incoming]
  clojure.lang.IDeref
  (deref [this] (current-value this)))

(prefer-method print-method java.util.Map clojure.lang.IDeref)

(defn reset-graph! []
  (reset! originators [])
  (reset! graph (loom/digraph)))

(defn- now [] (System/currentTimeMillis))

(defn all-successors
  [g nodes]
  (let [successors (partial loom/successors g)]
    (loop [all (into #{} nodes)
           curr nodes]
      (let [direct-successors
            (apply set/union
                   (c/map successors curr))]

        (if (empty? direct-successors)
          all
          (recur (set/union all direct-successors)
                 direct-successors))))))

(defn- add-originators
  [transaction-graph originators current-time]
  (loop [g transaction-graph
         o originators]
    (if (empty? o)
      g
      (let [{:keys [signal value]} (first o)]
        (recur
          (loom.attr/add-attr g signal :value [current-time value])
          (rest o))))))

(defn- recalculate-heights
  [g from-node]
  g)

(defn- height-sort
  [g nodes]
  (sort-by
    #(loom.attr/attr g % :height)
    nodes))

(defn- handle-node
  [originating-signals visited queue next-graph current-graph current-time]
  (let [node (first queue)
        remainder (rest queue)

        forward
        (fn [v]
          (loom.attr/add-attr
            next-graph node :value [current-time v]))

        rewire
        (fn [s-$]

          (let [fwd-$ (loom.attr/attr next-graph node :fwd-$)
                trigger-$ (first (into [] (loom/predecessors next-graph node)))
                prev-subgraph-$ (current-value current-graph trigger-$)
                after-node (loom/successors next-graph node)
                before-fwd (loom/predecessors next-graph fwd-$)


                updated-graph 
                (as-> next-graph <>
                  (apply loom/remove-edges <> 
                         (concat
                           (c/map (fn [n] [node n]) after-node)
                           (c/map (fn [n] [n fwd-$]) before-fwd)))
                  (loom/add-edges <> [node s-$] [s-$ fwd-$])
                  (recalculate-heights <> node))

                updated-queue
                (height-sort
                  updated-graph
                  (as-> remainder <>
                    (remove #(= % prev-subgraph-$) <>)
                    (conj <> s-$)))]

            [(conj visited node) updated-queue updated-graph]))

        incoming
        (if (empty? (:incoming node))
          (loom/predecessors next-graph node)
          (:incoming node))

        current-data
        (c/map #(loom.attr/attr current-graph % :value) incoming)

        incoming-data
        (c/map #(loom.attr/attr next-graph % :value) incoming)

        return (fn [g] [(conj visited node) remainder (or g next-graph)])]


    (cond
      (and
        (not-any? #(= % current-time) (c/map first incoming-data))
        (not (some #{node} originating-signals))
        (not= (:kind node) :prev))
      (do
        (return nil))

      (= (:kind node) :standard)
      (return 
        ((:proc node) forward incoming-data))

      (= (:kind node) :prev)
      (return
        ((:proc node) forward
         [[current-time 
           (current-value
             current-graph
             (loom.attr/attr current-graph node :source-$))]]))

      (= (:kind node) :switch)
      ((:proc node) rewire incoming-data)

      :else
      (return nil))))


(defn- propogate
  [graph originators current-time]
  (let [originating-signals (c/map :signal originators)
        successors (all-successors graph originating-signals)
        invalid-nodes
        (height-sort
          graph
          successors)]
    (loop [g (add-originators graph originators current-time)
           visited []
           invalid invalid-nodes]
      (if (empty? invalid)
        [g visited]
        (let [[new-visited new-queue new-graph]
              (handle-node
                originating-signals visited invalid g graph current-time)]
          (recur new-graph new-visited new-queue))))))

(defn propogate!
  []
  (let [[new-g visited] (propogate @graph @originators (now))
        prevs (apply
                set/union
                (c/map #(loom.attr/attr new-g % :prevs) visited))]

    (doseq [v visited]
      (let [subscribers (loom.attr/attr new-g v :subscribers)]
        (doseq [s subscribers]
          (s (current-value new-g v)))))

    (reset! graph new-g)
    (reset! originators 
            (c/map (fn [p] {:signal p
                            :value (current-value
                                     new-g 
                                     (loom.attr/attr new-g p :source-$))})
                   prevs))))

(defn transact!
  [thunk]
  (thunk)
  (propogate!)
  nil)

(defn >!
  [s-$ v]
  (swap! originators conj {:signal s-$ :value v}))

(defn close!
  [s-$])

(defn make-signal
  ([kind tag proc] (make-signal kind tag proc []))
  ([kind tag proc incoming]
   (let [heights
         (c/map #(loom.attr/attr @graph % :height) incoming)

         height
         (if (empty? heights)
           0
           (inc (apply max heights)))

         out-$
         (Signal. kind
                  tag
                  proc
                  incoming)]

     (swap! graph
            (fn [g]
              (as-> g <>
                (loom/add-nodes <> out-$)
                (apply loom/add-edges <>
                       (c/map (fn [from] [from out-$]) incoming))
                (loom.attr/add-attr
                  <> out-$ :height height))))

     (swap! originators conj {:signal out-$})

     out-$)))

(defn signal
  ([] (signal nil))
  ([v] (signal nil v))
  ([tag v] 
   (make-signal :standard tag (fn [forward _] (forward v)))))

(defn input
  [v]
  (let [out-$
        (make-signal :input nil nil)]
    (>! out-$ v)
    out-$))

(defn fwd
  []
  (make-signal :standard :fwd (fn [forward [[_ v]]] (forward v)) []))

(defn prev
  [s-$]
  (let [prev-$
        (make-signal :prev nil (fn [forward [[_ v]]]
                                 (forward v)) [])]

    (swap! graph
           (fn [g] 
             (-> g
                 (loom.attr/add-attr
                   prev-$ :source-$ s-$)
                 (loom.attr/add-attr
                   s-$
                   :prevs (set/union (loom.attr/attr g s-$ :prevs)
                                     #{prev-$})))))
    prev-$))
                                                          
(defn switch
  [s-$]
  (let [switch-$
        (make-signal :switch nil (fn [rewire [[_ v]]] (rewire v)) (if s-$ [s-$] []))

        fwd-$
        (fwd)]

    (swap! graph loom.attr/add-attr switch-$ :fwd-$ fwd-$)

    fwd-$))

(def flatten switch)

(defn subscribe!
  [s-$ proc]
  (swap!
    graph
    loom.attr/add-attr s-$ :subscribers
    (conj 
      (or
        (loom.attr/attr @graph s-$ :subscribers)
        #{})
      proc)))

(defn unsubscribe!
  [s-$ proc]
  (swap!
    graph
    loom.attr/add-attr s-$ :subscribers
    (disj
      (or
        (loom.attr/attr @graph s-$ :subscribers)
        #{})
      proc)))

(defn map
  [proc s-$]
  (make-signal :standard
               :map
               (fn [forward [[_ v]]]
                 (forward (proc v)))
               [s-$]))

(defn filter
  [pred s-$]
  (make-signal :standard
               :filter
               (fn [forward [[_ v]]]
                 (when (pred v)
                   (forward v)))
                 [s-$]))
 
(defn reduce
  [proc init s-$]
  (rec
    x
    (make-signal :standard
                 :reduce
                 (fn [forward [[_ acc] [_ v]]]
                   (forward (proc (or acc init) v)))
                 [(prev x)
                  s-$])))

(defn merge-simultaneous
  ([simultaneous-fn & signals]
   (make-signal
     :standard
     :merge
     (fn [forward values]
       (let [current-step (apply max (c/map first values))
             [_ v] (simultaneous-fn (c/filter
                                      (fn [[t]] (= t current-step))
                                      values))]
         (forward v)))
     signals)))

(def merge (partial merge-simultaneous first))

(defn zip
  [& signals]
  (make-signal
    :standard
    :zip
    (fn [forward values]
      (forward (mapv second values)))
    signals))

(defn sample-on
  [trigger-$ value-$]
  (make-signal
    :standard
    :sample-on
    (fn [forward [[vt value] [tt trigger]]]
      (when (>= tt vt) (forward value)))
    [value-$ trigger-$]))

(defn sample-when
  [trigger-$ value-$]
  (make-signal
    :standard
    :sample-when
    (fn [forward [[_ value] [_ trigger]]]
      (when trigger (forward value)))
    [value-$ trigger-$]))


(defn distinct
  [s-$]
  (make-signal
    :standard
    :distinct
    (fn [forward [[_ value] [_ previous]]]
      (when (not= value previous) (forward value)))
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

(defn choose
  [selection-$ options])

(defn slice
  [n s-$]
  (make-signal
    :standard
    :slice
    (fn [forward updates]
      (let [times (c/map first updates)
            values (c/map second updates)]
        (when (every? identity times)
          (forward (reverse values)))))
    (take n (iterate prev s-$))))

(defn count
  [s-$]
  (reduce (fn [c _] (inc c)) 0 s-$))

(defn partition
  [n s-$]
  (sample-on
    (filter
      #(zero? (mod % n))
      (count s-$))
    (slice 3 s-$)))

(defn changed-keys
  [s-$])

(defn throttle
  [s-$ ms])

(defn debounce
  [s-$ ms])

(defn delay
  [s-$ ms])
