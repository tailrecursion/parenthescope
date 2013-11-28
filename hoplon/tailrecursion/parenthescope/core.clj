(ns tailrecursion.parenthescope.core
  (:refer-clojure :exclude [accessor defn])
  (:require
    [tailrecursion.castra
     :refer [defn ex error *session*]]
    [tailrecursion.javelin-clj              :refer :all]
    [tailrecursion.parenthescope.edit       :refer :all]
    [clojure.walk                           :as walk]
    [tailrecursion.parenthescope.zip        :as zip]
    [tailrecursion.parenthescope.lang.text  :as text]))

(def dfl-keys
  {:normal
   {\h (op1 buffer zip/xy-left)
    \l (op1 buffer zip/xy-right)
    \j (op1 buffer zip/xy-down)
    \k (op1 buffer zip/xy-up)
    \^ (op1 buffer zip/xy-begin)
    \$ (op1 buffer zip/xy-end)
    \z #(do (push! modes :zipper) nil)}
   :zipper
   {\h (op1 buffer zip/left)
    \l (op1 buffer zip/right)
    \j (op1 buffer zip/down)
    \k (op1 buffer zip/up)
    \^ (op1 buffer zip/begin)
    \$ (op1 buffer zip/end)
    \z #(do (push! modes drop1) nil)}})

(defn init []
  (reset! config-keys dfl-keys)
  (push! buffer (string->zip :text "asdf\n\nqwer\nzxcv"))
  (swap! modes conj :normal))

(def allow (constantly true))

(defn mark-point [z]
  (-> (walk/postwalk #(if (map? %) (assoc % :point true) %) z)
      (with-meta (meta z))))

;(cell= (prn (and point [(meta (zip/root point)) (zip/node point)])))

(defc= printed  (when point (pprint (zip/edit point mark-point))))
(defc= state    {:mode mode :file-type file-type :point printed})

(defn get-state [[key & _]]
  {:rpc (allow)}
  (when key (process-key key))
  @state)
