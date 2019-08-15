(ns ulmus.transaction
  (:require [clojure.set :as set]))

(defonce ^:dynamic values (atom {}))

(defn >!
  [s-$ v]
  (swap! values assoc s-$ v))

(defn collect-invalid
  [nodes]
  (loop [invalid #{} generation nodes]
    (if (empty? generation)
      invalid
      (recur 
        (set/union invalid (set generation))
        (flatten (map (comp deref :outgoing) generation))))))

(defn visit-node
  [n values]
  (let [visit (fn [v] (assoc values n v))
        incoming-vals (map #(get values %) @(:incoming n))
        new-values ((:proc n) visit incoming-vals)]
    (or new-values values)))

(defn propogate
  [nodes initial-vals]
  (loop [remaining nodes
         values initial-vals]
    (if (empty? remaining)
      values 
      (recur (rest remaining)
             (visit-node 
               (first remaining)
               values)))))

(defn propogate!
  []
  (let [invalid (sort-by (comp deref :height)
                         (collect-invalid (keys @values)))
        new-values (propogate invalid @values)]
    (doseq [[n v] new-values]
      (reset! (:value n) v))
    (reset! values (atom {}))))
      
