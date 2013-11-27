(ns tailrecursion.parenthescope.edit
  (:require
    [tailrecursion.parenthescope.zip  :as zip]
    [tailrecursion.javelin-clj        :refer [cell cell= defc defc=]]))

(defn apply-stack [this x]
  (if-not (fn? x)
    (conj this x)
    (reduce #(apply-stack %1 %2) (empty this) (reverse (x this)))))

(defn push! [stack & xs]
  (swap! stack (partial reduce apply-stack) xs))

(defn drop1 [[x & xs :as items]]
  (if (seq xs) xs items))

(defc buffer      ())
(defc mode        ())
(defc config-keys {})

(defc= point      (->> buffer (drop-while (complement zip/zipper?)) first))
(defc= keymap     (get config-keys (first mode)))
(defc= dfl-key    (get keymap :default))

(defn set-last-col [x] (reset! zip/last-col x))

(cell= (when (and (nil? zip/last-col)
                  (zip/zipper? point)
                  (not (zip/branch? point)))
         (set-last-col (zip/col point))))

(defn prompt []
  (->> @point zip/node prn)
  (printf "%s > " (first @mode))
  (flush))

(defn do! [key]
  (if-let [f (or (get @keymap key) (and @dfl-key (@dfl-key key)))]
    (f)
    (printf "no key mapping for %s\n" (pr-str key))))
