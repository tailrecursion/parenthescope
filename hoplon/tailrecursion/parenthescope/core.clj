(ns tailrecursion.parenthescope.core
  (:refer-clojure :exclude [accessor defn])
  (:require
    [tailrecursion.castra
     :refer [defn ex error *session*]]
    [tailrecursion.javelin-clj              :refer :all]
    [tailrecursion.parenthescope.zip        :as zip]
    [tailrecursion.parenthescope.edit       :as edit]
    [tailrecursion.parenthescope.lang.text  :as text]))

(defn op1 [f] #(do (edit/push! edit/buffer (fn [[x & xs]] (conj xs (f x)))) nil))

(def left           (op1 zip/left))
(def begin          (op1 zip/begin))
(def right          (op1 zip/right))
(def end            (op1 zip/end))
(def up             (op1 zip/up))
(def down           (op1 zip/down))

(def xy-left        (op1 zip/xy-left))
(def xy-right       (op1 zip/xy-right))
(def xy-up          (op1 zip/xy-up))
(def xy-down        (op1 zip/xy-down))
(def xy-begin       (op1 zip/xy-begin))
(def xy-end         (op1 zip/xy-end))

(defn init []
  (reset!
    edit/config-keys
    {:normal
     {\h xy-left
      \l xy-right
      \j xy-down
      \k xy-up
      \^ xy-begin
      \$ xy-end
      \z #(do (edit/push! edit/mode :zipper) nil)}
     :zipper
     {\h left
      \l right
      \j down
      \k up
      \^ begin
      \$ end
      \z #(do (edit/push! edit/mode edit/drop1) nil)}})

  (swap! edit/mode conj :normal)
  (edit/push! edit/buffer (text/string->zip "asdf\n\nqwer\nzxcv")))

(def allow (constantly true))

(defn get-state [[key & _]]
  {:rpc (allow)}
  (when key (edit/do! key))
  (text/pprint @edit/point))

(init)
