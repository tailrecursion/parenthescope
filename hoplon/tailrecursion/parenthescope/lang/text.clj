(ns tailrecursion.parenthescope.lang.text
  (:require
    [fast-zip.core                    :as zip]
    [tailrecursion.parenthescope.zip  :as nav]
    [tailrecursion.parenthescope.edit :as edit]
    [clojure.walk                     :as walk]
    [clojure.string                   :as string]))

(defn make-node [x]
  (let [s (str x)
        t (if (string/blank? s) :empty :text)]
    {:style [t], :type :node, :text s}))

(defn node-maker [j]
  (fn [i x]
    (assoc (make-node x) :row j :col i)))

(defmethod edit/string->zip :text [_ x]
  (->>
    (string/split x #"\n")
    (map-indexed
      (fn [j x]
        (map-indexed (node-maker j) (or (seq x) [""]))))
    (#(with-meta % {:file-type :text}))
    zip/seq-zip
    zip/down
    zip/down))

(defmethod edit/pprint :text [z]
  (vec (mapcat identity (interpose ["\n"] (zip/root z)))))

(defmethod edit/zip->string :text [z]
  (->> z edit/pprint (map #(if (map? %) (:text %) %)) (apply str)))
