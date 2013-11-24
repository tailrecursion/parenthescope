(ns tailrecursion.parenthescope.main
  (:require
    [tailrecursion.parenthescope.edit       :as edit]
    [tailrecursion.parenthescope.lang.text  :as text]))

(defn init []
  (text/init)
  (edit/init! :text "asdf\nqwer\nzxcv"))

(defn do! [x]
  (edit/do! x))
