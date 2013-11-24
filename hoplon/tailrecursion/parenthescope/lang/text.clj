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
    {:style [t], :edit :text, :type :node, :text s}))

(defn node-maker [j]
  (fn [i x]
    (assoc (make-node x) :row j :col i)))

(defn mark-point [x]
  (walk/postwalk #(if (map? %) (assoc % :point true) %) x))

(defn string->zip [x]
  (->>
    (string/split x #"\n")
    (map-indexed
      (fn [j x]
        (map-indexed (node-maker j) (or (seq x) [""]))))
    zip/seq-zip
    zip/down
    zip/down))

(defn pprint [z]
  (let [z* (zip/edit z mark-point)]
    (vec (mapcat identity (interpose ["\n"] (zip/root z*))))))

(defn zip->string [z]
  (->> z pprint (map #(if (map? %) (:text %) %)) (apply str)))

(def edit-ops
  {:string->zip (edit/op [s] [(string->zip s)])
   :zip->string (edit/op [z] [(zip->string z)])
   :pprint      (edit/op [z] [(pprint z)])})

(defn init []
  (swap! edit/opmaps assoc-in [:edit :text] edit-ops))
