(ns tailrecursion.parenthescope
  (:require [tailrecursion.parenthescope.printers.clojure :refer [print-clojure]]
            [tailrecursion.parenthescope.printers :refer [object bounds page]]
            [clojure.zip :as z]
            [clojure.core.async :as async :refer [thread <! >! >!! <!! chan go]]
            [clojure.contrib.dataflow :as flow]
            [tailrecursion.warp :as ! :refer [throw! *e*]]
            [potemkin :refer [def-map-type]])
  (:import (javax.swing JFrame JPanel JTextArea)
           (java.awt Font Color)
           java.awt.event.KeyListener
           javax.swing.text.DefaultHighlighter$DefaultHighlightPainter
           javax.swing.text.Highlighter$HighlightPainter))

(def ^:mutable
  text (doto (JTextArea.)
         (.setEditable false)
         (.setFont (Font. Font/MONOSPACED Font/PLAIN 16))))

(defn show [^JTextArea text]
  (doto (JFrame. "parenthescope")
    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
    (.add text)
    (.pack)
    (.setLocationByPlatform true)
    (.setVisible true)))

(def bold-painter
  (reify Highlighter$HighlightPainter
    (paint [this g p0 p1 shape c]
      (.setColor g Color/ORANGE))))

(defn code-zip
  [root]
  (z/zipper (comp boolean '#{list vector} first)
            rest
            concat
            root))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def forms
  '[(list (symbol "ns") (symbol "fibonacci"))
    (list (symbol "def")
          (symbol "fib")
          (list (symbol "->>")
                (vector (long "0") (long "1"))
                (list (symbol "iterate")
                      (list (symbol "fn")
                            (vector (vector (symbol "a")
                                            (symbol "b"))
                                    (vector (symbol "b")
                                            (list (symbol "+")
                                                  (symbol "a")
                                                  (symbol "b"))))))
                (list (symbol "map") (symbol "first"))))])

(def zips (mapv code-zip forms))

(defn highlight! [^JTextArea text start end]
  (let [color (Color. 255 230 255)
        paint (DefaultHighlighter$DefaultHighlightPainter. color)
        hl (.getHighlighter text)]
    (.removeAllHighlights hl)
    (.addHighlight hl start end paint)))

(defn make-page
  [text form-zippers]
  (flow/build-dataflow
   [(flow/cell :source cursor  0)
    (flow/cell :source zippers form-zippers)
    (flow/cell current-zipper  (get ?zippers ?cursor))
    (flow/cell printed-zippers (mapv (comp print-clojure z/root) ?zippers))
    (flow/cell strings         (mapv str ?printed-zippers))
    (flow/cell offsets         (vec (cons 0 (map count ?strings))))
    (flow/cell current-printed (?printed-zippers ?cursor))
    (flow/cell highlight-bounds
               (->> (z/node ?current-zipper)
                    (bounds ?current-printed)
                    ((juxt :start :end))
                    (mapv (partial + (get ?offsets ?cursor)))))
    (flow/cell top?            (identical? (z/root ?current-zipper) (z/node ?current-zipper)))
    (flow/cell insert!         (.setText text (apply str ?strings)))
    (flow/cell highlight!      (apply highlight! text ?highlight-bounds))]))

(defn snapshot [df]
  (reduce (fn [m [k v]]
            (assoc m (keyword (name k)) @(:value (first v)))) {} (:cells-map @df)))

(defn nav!
  [page zipf]
  (let [{:keys [zippers cursor]} (snapshot page)]
    (flow/update-values page {'zippers (update-in zippers [cursor] zipf)})))

(defn prev!
  [page]
  (let [{:keys [zippers cursor]} (snapshot page)]
    (flow/update-values page {'cursor (if (zero? cursor)
                                        (dec (count zippers))
                                        (dec cursor))})))

(defn next!
  [page]
  (let [{:keys [zippers cursor]} (snapshot page)]
    (flow/update-values page {'cursor (if (< cursor (dec (count zippers)))
                                        (inc cursor)
                                        0)})))

(defn top? [page]
  (flow/get-value page 'top?))

(def codes {38 :up, 39 :right, 40 :down, 37 :left, 33 :pgup, 34 :pgdown, })

(defn listen! [text page]
  (doto text
    (.addKeyListener (reify KeyListener
                       (keyPressed [_ e]
                         (case (codes (:keyCode (bean e)))
                           :up    (if (top? page)
                                    (prev! page)
                                    (nav! page z/up))
                           :down  (if (top? page)
                                    (next! page)
                                    (nav! page #(if (and (z/branch? %) (seq (z/children %))) (z/down %) %)))
                           :left  (if (top? page)
                                    (prev! page)
                                    (nav! page #(if-let [left (z/left %)] left (z/up %))))
                           :right (if (top? page)
                                    (nav! page #(if (and (z/branch? %) (seq (z/children %))) (z/down %) %))
                                    (nav! page #(if-let [right (z/right %)] right (z/up %))))
                           (println "Unhandled key code:" (:keyCode (bean e)))))
                       (keyReleased [_ e])
                       (keyTyped [_ e])))))

(defn demo [& args]
  (def p (make-page text zips))
  (listen! text p)
  (show text)
  (.append text "\n;; navigate with arrow keys"))
