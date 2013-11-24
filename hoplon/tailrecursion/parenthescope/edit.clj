(ns tailrecursion.parenthescope.edit
  (:require
    [clojure.pprint                   :as pprint]
    [tailrecursion.parenthescope.zip  :as zip]))

(def ops-stack    (atom []))
(def mode         (atom nil))
(def file-type    (atom nil))
(def point-stack  (atom []))

(defn pop-stack! [stack n]
  (let [ret (take-last n @stack)]
    (swap! stack #(vec (drop-last n %)))
    ret))

(defn push-stack! [stack xs]
  (swap! stack into xs))

(defmacro op [arglist & body]
  `(fn [stack#]
     (->>
       (apply
         (fn ~arglist ~@body)
         (reverse (pop-stack! stack# ~(count arglist))))
       reverse
       (push-stack! stack#))))

(defmacro defop [name arglist & body]
  `(def ~name (op ~arglist ~@body)))

(defn evaluate! [stack]
  (loop [s stack]
    (if-let [f (zip/func? (peek @s))]
      (do
        (pop-stack! s 1)
        (f (evaluate! s))
        (recur s))
      s)))

(defn doit []
  (evaluate! point-stack)
  (->> @point-stack (mapv #(if (zip/zipper? %) (zip/node %) %)) pprint/pprint)
  (printf "%s %s => " @file-type (:mode (peek @ops-stack))))

(defn pop-mode! [n]
  (swap! ops-stack #(if (< n (count %)) (vec (drop-last n %)) %))
  (doit))

(def opmaps (atom {}))

(def opmaps
  (atom {:normal
         {:done         (fn [])
          :right        (op [z] [(zip/nav-right   z)])
          :left         (op [z] [(zip/nav-left    z)])
          :down         (op [z] [(zip/nav-down    z)])
          :up           (op [z] [(zip/nav-up      z)])
          :out          (op [z] [(zip/nav-out     z)])
          :in           (op [z] [(zip/nav-in      z)])
          :delete       (op [z] [(zip/nav-delete  z)])
          :zip->string  (op [z] [(get-in @opmaps [:edit @file-type :zip->string]) z])
          :string->zip  (op [z] [(get-in @opmaps [:edit @file-type :string->zip]) z])}
         :edit {}}))

(def keymaps
  (atom {:normal {\~ :done
                  \l :right
                  \h :left
                  \j :down
                  \k :up
                  \[ :out
                  \] :in
                  \x :delete
                  \p :zip->string
                  \e :string->zip}
         :edit {}}))

(defn push-mode! [x]
  (if-let [y (get @opmaps x)]
    (push-stack! ops-stack [(assoc y :mode x)])
    (printf "no such mode: %s\n" x))
  (doit))

(defn init! [type x]
  (reset! file-type type)
  (push-mode! :normal)
  (if-let [op (get-in @opmaps [:edit type :string->zip])]
    (push-stack! point-stack [x op])
    (printf "no :string->zip associated with file-type: %s\n" type))
  (doit))

(defn do! [x]
  (let [opz (peek @ops-stack)]
    (if-let [op (get-in @keymaps [(:mode opz) x])]
      (if-let [f (get opz op)]
        (push-stack! point-stack [f])
        (printf "no op mapping: %s\n" op))
      (printf "no key binding: '%s'\n" x))
    (doit)))
