(ns tailrecursion.parenthescope
  (:require [tailrecursion.parenthescope.printers.clojure :refer [print-clojure]]
            [tailrecursion.parenthescope.printers :refer [object bounds page]]
            [clojure.zip :as z])
  (:import (javax.swing JFrame JPanel JTextArea)
           (java.awt Font Color)
           javax.swing.text.DefaultHighlighter$DefaultHighlightPainter
           javax.swing.text.DefaultHighlighter))

;; (def ^:mutable
;;   text (doto (JTextArea.)
;;          (.setEditable false)
;;          (.setFont (Font. Font/MONOSPACED Font/PLAIN 16))))

;; (defn show [^JTextArea text]
;;   (doto (JFrame. "parenthescope")
;;     (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
;;     (.add text)
;;     (.pack)
;;     (.setLocationByPlatform true)
;;     (.setVisible true)))
;; (defn highlight! [{:keys [start end]}]
;;   (let [orange (DefaultHighlighter$DefaultHighlightPainter. Color/ORANGE)
;;         hl (.getHighlighter text)]
;;     (.removeAllHighlights hl)
;;     (.addHighlight hl start end orange)))

;; (defn code-zip
;;   [root]
;;   (z/zipper (comp boolean '#{list vector} first)
;;             rest
;;             concat
;;             root))

;; (def demo-code
;;   '(list
;;     (list
;;      (symbol "defn")
;;      (symbol "pid!")
;;      (vector)
;;      (list
;;       (symbol "->>")
;;       (list
;;        (symbol "..")
;;        (symbol "java.lang.management.ManagementFactory")
;;        (symbol "getRuntimeMXBean")
;;        (symbol "getName"))
;;       (list
;;        (symbol "take-while")
;;        (list
;;         (symbol "partial")
;;         (symbol "not=")
;;         (char "@")))
;;       (list
;;        (symbol "apply")
;;        (symbol "str"))))))

;; (def ^:mutable code (atom (code-zip demo-code)))

;; (defn nav! [f]
;;   (highlight! (get object->idx (z/node (swap! code f)))))

;; (defn demo []
;;   (show text)
;;   (def tw (text-writer text))
;;   ;; todo: write to stringbuffer or similar and setText on init
;;   (time (binding [*out* tw] (pprint (z/root @code))))
;;   (highlight! (get object->idx (z/root @code)))
;;   (doseq [f [z/down
;;              z/down
;;              z/rightmost
;;              z/down
;;              z/right
;;              z/right
;;              z/down
;;              z/right
;;              z/up
;;              z/up
;;              z/up
;;              z/up]]
;;     (Thread/sleep 400)
;;     (println f)
;;     (nav! f)))
