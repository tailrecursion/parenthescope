(ns tailrecursion.parenthescope.printers
  (:import java.util.WeakHashMap))

(defprotocol IPagingBuffer
  (object [_ point] "Index of the top-most object at point.")
  (bounds [_ object] "Pair of the start and end points of object, inclusive.")
  (page [_ start max] "PagingBuffer containing all fully-printed objects from start up to max, inclusive."))

(defn next-index [index starting-at size]
  (when-let [kvs (->> index
                      (drop-while (fn [[k]] (< k starting-at)))
                      (take size)
                      (map second)
                      (map vector (range)))]
    (into (sorted-map) kvs)))

(defn next-bounds [bounds starting-at size]
  (->> bounds
       (filter (fn [[obj {:keys [start end]}]]
                 (or (and (>= start starting-at) (< start (+ starting-at size)))
                     (and (>= end starting-at) (< end (+ starting-at size))))))
       (reduce (fn [wm [o {:keys [start end]}]]
                 (doto wm (.put o {:start (- start starting-at)
                                   :end (- end starting-at)})))
               (WeakHashMap.))))

(defrecord PagingBuffer [s index bounds]
  IPagingBuffer
  (object [_ point]
    (first (get index point)))
  (bounds [_ o]
    (get bounds o))
  (page [_ starting-at size]
    (when-let [new-index (next-index index starting-at size)]
      (let [new-bounds (next-bounds bounds starting-at size)]
        (PagingBuffer. (.substring s starting-at) new-index new-bounds))))
  Object
  (toString [_] s))

(defn make-buffer [s index ^WeakHashMap bounds]
  (PagingBuffer. s index bounds))
