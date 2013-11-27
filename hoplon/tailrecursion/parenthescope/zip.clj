(ns tailrecursion.parenthescope.zip
  (:refer-clojure :exclude [type])
  (:require
    [fast-zip.core              :as zip]
    [clojure.pprint             :as pprint]
    [tailrecursion.javelin-clj  :refer [defc defc= cell cell=]]))

(defc last-col nil)

(def zipclass
  (class (zip/zipper nil nil nil nil)))

(defn func?     [x] (and (fn? x) x))
(defn zipper?   [x] (and x (instance? zipclass x) x))
(defn root?     [z] (and z (= (zip/root z) (zip/node z)) z))
(defn branch?   [z] (and (zip/branch? z) z))
(defn rights?   [z] (and (seq (zip/rights z)) z))
(defn lefts?    [z] (and (seq (zip/lefts z)) z))

(defn root      [z] (when (zipper? z) (zip/root z)))
(defn node      [z] (when (zipper? z) (zip/node z)))
(defn siblings  [z] (when (zipper? z) (concat (zip/lefts z) (zip/rights z))))

(defn col       [z] (:col   (zip/node z)))
(defn row       [z] (:row   (zip/node z)))
(defn text      [z] (:text  (zip/node z)))
(defn style     [z] (:style (zip/node z)))
(defn edit      [z] (:edit  (zip/node z)))
(defn type      [z] (:type  (zip/node z)))
(defn colr      [z] (let [n (zip/node z)] (+ (:col n) (count (:text n)))))

(defn left      [z] (or (zip/left       z) z))
(defn leftmost  [z] (or (zip/leftmost   z) z))
(defn begin     [z] (if (lefts? z) (begin (zip/left z)) z))
(defn right     [z] (or (zip/right      z) z))
(defn rightmost [z] (or (zip/rightmost  z) z))
(defn end       [z] (if (rights? z) (end (zip/right z)) z))
(defn up        [z] (or (zip/up         z) z))
(defn down      [z] (or (zip/down       z) z))

(defn root-loc [z]
  (loop [z z] (or (root? z) (recur (zip/up z)))))

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

(defn max<=   [v xs]  (->> xs sort (take-while #(<= % v)) last))
(defn min>=   [v xs]  (->> xs sort (drop-while #(< % v)) first))
(defn set-col [z]     (when z (reset! last-col (col z)) z))

(defn xy-left   [z] (or (set-col (xy-nav z #(max<= (row z) %) #(max<= (dec (col z)) %))) z))
(defn xy-right  [z] (or (set-col (xy-nav z #(max<= (row z) %) #(min>= (colr z) %))) z))
(defn xy-up     [z] (or (xy-nav z #(max<= (dec (row z)) %) #(max<= @last-col %)) z))
(defn xy-down   [z] (or (xy-nav z #(min>= (inc (row z)) %) #(max<= @last-col %)) z))
(defn xy-begin  [z] (or (set-col (xy-nav z #(max<= (row z) %) #(apply min %))) z))
(defn xy-end    [z] (or (set-col (xy-nav z #(max<= (row z) %) #(apply max %))) z))

