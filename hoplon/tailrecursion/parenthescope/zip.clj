(ns tailrecursion.parenthescope.zip
  (:refer-clojure :exclude [type])
  (:require
    [fast-zip.core    :as zip]
    [clojure.pprint   :as pprint]))

(def zipclass
  (class (zip/zipper nil nil nil nil)))

(defn func?     [x] (and (fn? x) x))
(defn zipper?   [x] (and x (instance? zipclass x) x))
(defn root?     [z] (and z (= (zip/root z) (zip/node z)) z))
(defn branch?   [z] (and (zip/branch? z) z))
(defn rights?   [z] (and (seq (zip/rights z)) z))
(defn lefts?    [z] (and (seq (zip/lefts z)) z))

(defn node      [z] (when z (zip/node z)))
(defn siblings  [z] (when z (concat (zip/lefts z) (zip/rights z))))

(defn col       [z] (:col   (zip/node z)))
(defn row       [z] (:row   (zip/node z)))
(defn text      [z] (:text  (zip/node z)))
(defn style     [z] (:style (zip/node z)))
(defn edit      [z] (:edit  (zip/node z)))
(defn type      [z] (:type  (zip/node z)))
(defn colr      [z] (let [n (zip/node z)] (+ (:col n) (count (:text n)))))

(defn loc=
  ([x y]
   (and (= (col x) (col y)) (= (row x) (row y))))
  ([x y & more]
   (reduce loc= (list* x y more))))

(defn upmost [z]
  (loop [z z]
    (let [z* (zip/up z)]
      (if-not z* z (recur z*)))))

(defn zip-seq [z]
  (loop [z* (zip/next z), zs [z]]
    (or (and (root? z*) zs)
        (recur (zip/next z*) (conj zs z*)))))

(defn nav-until [z break? f]
  (loop [z z] (if (break? z) z (recur (f z)))))

(defn nav-right [z]
  (if (branch? z)
    (or (zip/right z) z)
    (loop [z* (zip/next z)]
      (cond (root? z*)        z
            (zip/branch? z*)  (recur (zip/next z*))
            :else             z*))))

(defn nav-left [z]
  (if (branch? z)
    (or (zip/left z) z)
    (loop [z* (zip/prev z)]
      (cond (nil? z*)         z
            (zip/branch? z*)  (recur (zip/prev z*))
            :else             z*))))

(defn nav-down [z]
  (if (branch? z)
    (nav-right z)
    (let [[y x] ((juxt row col) z)]
      (->> (upmost z) zip-seq (filter #(and (row %) (< y (row %))))
           (group-by row) (#(when (seq %) (get % (apply min (keys %)))))
           (sort-by #(Math/abs (- x (col %)))) first (#(or % z))))))

(defn nav-up [z]
  (if (branch? z)
    (nav-left z)
    (let [[y x] ((juxt row col) z)]
      (->> (upmost z) zip-seq (filter #(and (row %) (> y (row %))))
           (group-by row) (#(when (seq %) (get % (apply max (keys %)))))
           (sort-by #(Math/abs (- x (col %)))) first (#(or % z))))))

(defn nav-out [z]
  (let [z* (zip/up z)]
    (if (root? z*) z z*)))

(defn nav-in [z]
  (if-not (branch? z) z (zip/down z)))

(defn nav-delete [z]
  (zip/remove z))
