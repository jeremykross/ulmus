(ns ulmus.transaction
  (:require [clojure.set :as set]))

(defonce values (atom {}))
(defonce prevs (atom {}))
(defonce subscriptions (atom {}))

; probably don't want this
; just let unswitched nodes get visited
; won't chnage their values.
; the logic here will fail in many cases
(defn- union-with-count
  [a b]
  (let [against-count (fn [x] [x (or (:count (meta x)) 1)])
        a-cnt (into {} (map against-count a))
        b-cnt (into {} (map against-count b))]
    (map
      (fn [[v cnt]]
        (with-meta v
                   {:count cnt}))
      (merge-with + a-cnt b-cnt))))

(defn remove-edge!
  [src-$ dest-$]
  (swap! 
    (:outgoing src-$)
    (fn [outgoing]
      (remove
        #(= % dest-$)
        outgoing)))
  (swap!
    (:incoming dest-$)
    (fn [incoming]
      (remove
        #(= % src-$)
        incoming)))
  nil)

(defn add-edge!
  [src-$ dest-$]
  (swap!
    (:outgoing src-$)
    conj dest-$)
  (swap!
    (:incoming dest-$)
    conj src-$)
  nil)

(defn >!
  [s-$ v]
  (swap! values assoc s-$ v))

(defn prev!
  [prev-$ src-$]
  (swap! prevs #(merge-with concat % {src-$ [prev-$]})))

(defn pipe!
  [src-$ dest-$]
  (swap! (:outgoing src-$) conj dest-$)
  (reset! (:incoming dest-$) [src-$])
  (reset! (:height dest-$) (inc @(:height src-$)))
  (>! dest-$ @src-$))

(defn subscribe!
  [s-$ proc]
  (swap! subscriptions #(merge-with concat {s-$ [proc]} %)))

(defn collect-invalid
  [nodes]
  (loop [invalid #{} generation nodes]
    (if (empty? generation)
      invalid
      (recur 
        (set/union invalid (set generation))
        (flatten (map (comp deref :outgoing) generation))))))

(defn recalculate-height!
  [s-$]
  (reset! (:height s-$)
          (inc (apply max (map (comp deref :height) @(:incoming s-$))))))

(defn recalculate-heights!
  [sigs]
  (doseq [s-$ sigs]
    (recalculate-height! s-$)))

(defn height-sort
  [nodes]
  (sort-by (comp deref :height) nodes))


(defn visit-standard
  [values n]
  (let [visit (fn [v]
                (assoc values n v))
        incoming-vals (map #(or (get values %) @(:value %)) @(:incoming n))
        new-values ((:proc n) visit incoming-vals)]
    (or new-values values)))

(defn visit-switch!
  [values remaining n]
  (let [incoming-vals (map #(or (get values %) @(:value %)) @(:incoming n))

        target-$ (first @(:outgoing n))

        wired? (not= (:tag target-$) :forward)

        rewire!
        (fn [v]
          (when v
            (if wired?
              (let [fwd-$ (first @(:outgoing target-$))]
                (remove-edge! n target-$)
                (remove-edge! target-$ fwd-$)
                (add-edge! v fwd-$))
              (let [fwd-$ target-$]
                (remove-edge! n fwd-$)
                (add-edge! v fwd-$)))
            (add-edge! n v)
            true))

        newly-remaining
        (if ((:proc n) rewire! incoming-vals)
          (let [from-new-target (collect-invalid incoming-vals)]
            (height-sort (set/union from-new-target remaining)))
          remaining)]

    ; recalc heights here?

    newly-remaining))

(defn visit-node
  [remaining values]
  (let [n (first remaining)
        others (rest remaining)]
    (condp = (:kind n)
      :standard [others (visit-standard values n)]
      :switch [(visit-switch! values others n) values]
      [others values])))

(defn propogate
  [nodes initial-vals]
  (loop [visited []
         remaining nodes
         values initial-vals]
    (if (empty? remaining)
      values 
      (let [[next-remaining next-values]
            (visit-node remaining values)]
        (recur 
          (conj visited (first remaining))
          next-remaining
          next-values)))))

(defn propogate!
  []
  (let [invalid (height-sort (collect-invalid (keys @values)))
        new-values (propogate invalid @values)
        prevs (flatten (remove nil? (map #(get @prevs %) (keys new-values))))]

    (doseq [[n v] new-values]
      (reset! (:value n) v))

    (reset! values
            (into
              {}
              (map (fn [prev] [prev @(:tag prev)])
                   prevs)))

    (doseq [[n v] new-values]
      (doseq [subscription (get @subscriptions n)]
        (subscription v)))

    nil))
