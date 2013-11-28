(ns tailrecursion.parenthescope.lang.text
  (:require
    [fast-zip.core                    :as zip]
    [clojure.string                   :as string]
    [tailrecursion.parenthescope.edit :as edit]))

(defn make-node [x]
  {:style [(if (= "" (str x)) :empty :text)], :type :node, :text (str x)})

(defn node-maker [j]
  (fn [i x] (assoc (make-node x) :row j :col i)))

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
