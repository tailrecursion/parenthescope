(ns tailrecursion.parenthescope.printers.clojure
  (:require [fipp.printer :refer [pprint-document]]
            [tailrecursion.parenthescope.printers :refer [make-buffer]])
  (:import java.util.WeakHashMap
           java.io.StringWriter))

(defn assoc-in! [^WeakHashMap m [k & ks] v]
  (doto m (.put k (assoc-in (.get m k) ks v))))

(declare ^:dynamic *count*
         ^:dynamic *bounds*
         ^:dynamic *index*)

(defmulti pprint first)

(defn start! [obj]
  (let [point (*count*)]
    (assoc-in! *bounds* [obj :start] point)
    point))

(defn merge-index [index start end obj]
  (let [points (map vector (range start end) (repeat obj))]
    (reduce (fn [m [i o]] (update-in m [i] (fnil conj []) o)) index points)))

(defn end! [start obj]
  (let [end (*count*)]
    (assoc-in! *bounds* [obj :end] end)
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

(defn print-clojure [code & [options]]
  (let [sw (StringWriter.)
        countfn #(.length (str sw))
        bounds (WeakHashMap.)
        index (atom (sorted-map))]
    (binding [*count* countfn
              *bounds* bounds
              *index* index
              *out* sw]
      (pprint-document (pprint code) (merge options defaults))
      (make-buffer (str sw) @index bounds))))

(comment
  (require '[clojure.pprint :as pp])
  (require '[tailrecursion.parenthescope.printers :refer [object bounds page]])
  (def code '(list (symbol "+") (long "1") (long "2")))
  (def b (print-clojure code))
  (println (str b))
  (pp/pprint b))
