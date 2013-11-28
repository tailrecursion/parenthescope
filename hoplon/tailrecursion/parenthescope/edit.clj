(ns tailrecursion.parenthescope.edit
  (:refer-clojure :exclude [accessor])
  (:require
    [fast-zip.core              :as zip]
    [tailrecursion.javelin-clj  :refer :all]))

(def zipclass   (class (zip/zipper nil nil nil nil)))
(def zipper?    #(and % (instance? zipclass %) %))
(def first-arg* (fn [x & _] x))
(def file-type* (comp :file-type meta zip/root))

(defmulti string->zip first-arg*)
(defmulti zip->string file-type*)
(defmulti pprint      file-type*)

(defmethod pprint :default [z] (prn [:debug (file-type* z) (meta (zip/root z)) (zip/root z)]) (zip/root z))

(defn apply-stack [this x]
  (if-not (fn? x)
    (conj this x)
    (reduce #(apply-stack %1 %2) (empty this) (reverse (x this)))))

(defn push! [stack & xs]
  (swap! stack (partial reduce apply-stack) xs))

(defn drop1 [[x & xs :as items]]
  (if (seq xs) xs items))

(defn op1 [stack f]
  #(do (push! stack (fn [[x & xs]] (conj xs (f x)))) nil))

(defc buffer        ())
(defc modes         ())
(defc config-keys   {})

(defc= mode         (first modes))
(defc= point        (->> buffer (drop-while (complement zipper?)) first))
(defc= file-type    (and (zipper? point) (file-type* point)))
(defc= keymap       (get config-keys mode))
(defc= default-key  (get keymap :default))

(defn process-key [key]
  (if-let [f (or (get @keymap key) (and @default-key (@default-key key)))]
    (f)
    (throw (Exception. (format "no key mapping for %s\n" (pr-str key))))))
