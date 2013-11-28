(ns tailrecursion.parenthescope.zip
  (:refer-clojure :exclude [accessor type])
  (:require
    [fast-zip.core                    :as zip]
    [clojure.pprint                   :as pprint]
    [tailrecursion.javelin-clj        :refer :all]
    [tailrecursion.parenthescope.edit :as edit]))

(declare set-col)

(defc last-col  nil)

(let [init #(set-col @edit/point)]
  (cell= (when (seq edit/modes) (init))))

(def  zipclass  (class (zip/zipper nil nil nil nil)))

(defn func?     [x] (and (fn? x) x))
(defn zipper?   [x] (and x (instance? zipclass x) x))
(defn root?     [z] (and z (= (zip/root z) (zip/node z)) z))
(defn branch?   [z] (and (zip/branch? z) z))
(defn rights?   [z] (and (seq (zip/rights z)) z))
(defn lefts?    [z] (and (seq (zip/lefts z)) z))

(defn root      [z] (when (zipper? z) (zip/root z)))
(defn root-loc  [z] (loop [z z] (or (root? z) (recur (zip/up z)))))
(defn node      [z] (when (zipper? z) (zip/node z)))
(defn siblings  [z] (when (zipper? z) (concat (zip/lefts z) (zip/rights z))))
(defn edit      [z f & args] (when (zipper? z) (apply zip/edit z f args)))

(defn col       [z] (:col   (zip/node z)))
(defn row       [z] (:row   (zip/node z)))
(defn text      [z] (:text  (zip/node z)))
(defn style     [z] (:style (zip/node z)))
(defn type      [z] (:type  (zip/node z)))
(defn colr      [z] (let [n (zip/node z)] (+ (:col n) (count (:text n)))))

(defn set-col   [z] (when z (reset! last-col (col z)) z))

(defn left      [z] (or (set-col (zip/left      z)) z))
(defn leftmost  [z] (or (set-col (zip/leftmost  z)) z))
(defn right     [z] (or (set-col (zip/right     z)) z))
(defn rightmost [z] (or (set-col (zip/rightmost z)) z))
(defn up        [z] (or (set-col (zip/up        z)) z))
(defn down      [z] (or (set-col (zip/down      z)) z))
(defn begin     [z] (if (lefts?  z) (set-col (begin (zip/left  z))) z))
(defn end       [z] (if (rights? z) (set-col (end   (zip/right z))) z))

(defn zip-seq [z]
  (let [z (root-loc z)]
    (loop [z (zip/next z), ret [z]]
      (cond (zip/end? z)  (conj ret z)
            (root? z)     z
            :else         (recur (zip/next z) (conj ret z))))))

(defn xy-seq [z]
  (->> (zip-seq z)
    (remove zip/branch?)
    (reduce #(assoc %1 [(row %2) (col %2)] %2) {})))

(defn xy-nav [z row? col?]
  (or (branch? z)
      (let [xy  (xy-seq z)]
        (when-let [r (row? (map first (keys xy)))]
          (get xy [r (col? (map second (filter #(= r (first %)) (keys xy))))])))))

(defn max<= [v xs] (when (and v (seq xs)) (->> xs sort (take-while #(<= % v)) last)))
(defn min>= [v xs] (when (and v (seq xs)) (->> xs sort (drop-while #(< % v)) first)))
(defn sel=  [v xs] (when (and v (seq xs)) (->> xs (drop-while #(not= % v)) first)))

(defn xy-left   [z] (or (set-col (xy-nav z #(sel= (row z) %) #(max<= (dec (col z)) %))) z))
(defn xy-right  [z] (or (set-col (xy-nav z #(sel= (row z) %) #(min>= (colr z) %))) z))
(defn xy-up     [z] (or (xy-nav z #(max<= (dec (row z)) %) #(max<= @last-col %)) z))
(defn xy-down   [z] (or (xy-nav z #(min>= (inc (row z)) %) #(max<= @last-col %)) z))
(defn xy-begin  [z] (or (set-col (xy-nav z #(sel= (row z) %) #(apply min %))) z))
(defn xy-end    [z] (or (set-col (xy-nav z #(sel= (row z) %) #(apply max %))) z))
