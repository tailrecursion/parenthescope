(ns tailrecursion.parenthescope.printers.clojure
  (:require [fipp.printer :refer [pprint-document]]
            [tailrecursion.parenthescope.printers :refer [make-buffer]]
            [potemkin :refer [def-map-type]])
  (:import java.util.WeakHashMap
           java.lang.ref.WeakReference
           java.io.StringWriter))

(defn scan [f v]
  (->> (range)
       (map vector v)
       (drop-while (comp (complement f) first))
       first
       second))

(def-map-type IdenticalPList [m]
  (get [_ k default]
       (second (first (filter (comp (partial identical? k) first) (get m k)))))
  (assoc [_ k v]
    (IdenticalPList. (update-in m [k] (fnil conj ()) [k v])))
  (dissoc [_ k])
  (keys [_] (keys m)))

(declare
 ^{:doc "Thunk; returns the current length of the output buffer."
   :dynamic true}
 *count*
 ^{:doc "WeakHashMap of objects to maps of :start/:end indexes within the output buffer."
   :dynamic true}
 *bounds*
 ^{:doc "Sorted map of indexes in the output buffer to a vector of objects at index in visibility order."
   :dynamic true}
 *index*)

(defmulti pprint first)

(defn start! [obj]
  (let [point (*count*)]
    (swap! *bounds* assoc-in [obj :start] point)
    point))

(defn merge-index
  [index start end obj]
  (let [points (map vector (range start end) (repeat obj))]
    (reduce (fn [m [i o]] (update-in m [i] (fnil conj []) o)) index points)))

(defn end! [start obj]
  (let [end (*count*)]
    (swap! *bounds* assoc-in [obj :end] end)
    (swap! *index* merge-index start end obj)))

(defmacro relate [representation obj]
  `(let [obj# ~obj, start# (promise)]
     [:group
      [:call #(deliver start# (start! obj#))]
      ~representation
      [:call #(end! @start# obj#)]]))

(defmethod pprint 'symbol [[_ s :as obj]]
  (relate [:text s] obj))

(defmethod pprint 'long [[_ s :as obj]]
  (relate [:text s] obj))

(defmethod pprint 'char [[_ s :as obj]]
  (relate [:text (str "\\" s)] obj))

(defmethod pprint 'comment [[_ s :as obj]]
  (relate [:text (str ";; " s)] obj))

(defn pprint-coll [l r obj contents]
  (relate
   [:group (concat [[:text l]]
                   (interpose :line (map pprint contents))
                   [[:text r]])]
   obj))

(defmethod pprint 'list [[_ & contents :as obj]]
  (pprint-coll "(" ")" obj contents))

(defmethod pprint 'vector [[_ & contents :as obj]]
  (pprint-coll "[" "]" obj contents))

(def defaults {:width 80})

(defn print-clojure [form & [options]]
  (let [sw (StringWriter.)
        countfn #(.length (str sw))
        bounds (atom (IdenticalPList. {}))
        index (atom (sorted-map))]
    (binding [*count* countfn
              *bounds* bounds
              *index* index
              *out* sw]
      (pprint-document (pprint form) (merge options defaults))
      (make-buffer (str sw) @index @bounds))))

(comment
  (require '[clojure.pprint :as pp])
  (require '[tailrecursion.parenthescope.printers :refer [object bounds page]])
  (def forms
    '((list (symbol "ns") (symbol "fibonacci"))
      (list (symbol "def")
            (symbol "fib")
            (list (symbol "->>")
                  (vector (long "0") (long "1"))
                  (list (symbol "iterate")
                        (list (symbol "fn")
                              (vector (vector (symbol "a")
                                              (symbol "b"))
                                      (vector (symbol "b")
                                              (list (symbol "+")
                                                    (symbol "a")
                                                    (symbol "b"))))))
                  (list (symbol "map") (symbol "first"))))))
  (def code '(list (symbol "+") (long "1") (long "2")))
  (def b (print-clojure code))
  (println (str b))
  (pp/pprint b))
