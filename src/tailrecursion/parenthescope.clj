(ns tailrecursion.parenthescope
  (:require [fipp.printer :as p :refer [defprinter]]
            [clojure.zip :as z])
  (:import (javax.swing JFrame JPanel JTextArea)
           javax.swing.text.DefaultHighlighter$DefaultHighlightPainter
           java.awt.Color
           java.io.Writer
           javax.swing.text.DefaultHighlighter))

(def text (doto (JTextArea.)
            (.setEditable false)))

(def delims {"\uFFFE" :start "\uFFFF" :end})

(defn text-writer [text atm]
  (proxy [Writer] []
    (write [x]
      (condp = (class x)
        String (if (delims x)
                 (swap! atm assoc-in [(delims x)] (.length (.getText text)))
                 (.append text x))
        Integer (.append text (str (char x)))
        (throw (UnsupportedOperationException. (str "can't write " (class x))))))
    (flush [])
    (close [])))

(defn show [^JTextArea text]
  (doto (JFrame. "parenthescope")
    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
    (.add text)
    (.pack)
    (.setLocationByPlatform true)
    (.setVisible true)))

(def point (atom nil))

(defn track [printed obj]
  (if (identical? @point obj)
    [:group [:pass "\uFFFE"] printed [:pass "\uFFFF"]]
    printed))

(defmulti pp (comp name first))

(defmethod pp "symbol" [[_ s :as obj]]
  (track [:text s] obj))

(defmethod pp "long" [[_ s :as obj]]
  (track [:text s] obj))

(defmethod pp "list" [[_ & contents :as obj]]
  (track
   [:group (concat [[:text "("]]
                   (interpose :line (map pp contents))
                   [[:text ")"]])]
   obj))

(defprinter pprint pp {:width 80})

(def orange
  (DefaultHighlighter$DefaultHighlightPainter. Color/ORANGE))

(defn refresh! [text code]
  (let [selection (atom {})
        hl (.getHighlighter text)]
    (.removeAllHighlights hl)
    (.setText text nil)
    (binding [*out* (text-writer text selection)]
      (pprint code))
    (.addHighlight hl (:start @selection) (:end @selection) orange)))

(defn code-zip
  [root]
  (z/zipper (comp boolean #{'clj/list} first)
            rest
            concat
            root))

(def code
  (atom
   (code-zip
    '(clj/list
      (clj/symbol "+")
      (clj/long "1")
      (clj/long "2")))))

(defn init! []
  (reset! point (z/root @code))
  (refresh! text (z/root @code)))

(defn nav! [f]
  (swap! code f)
  (reset! point (z/node @code))
  (refresh! text (z/root @code)))

(comment

  (show text)
  (init!)
  (doseq [f [z/down z/right z/right z/up]]
    (nav! f)
    (Thread/sleep 1000))

  )

;; (comment
;;   (defn pid! []
;;     (->> (.. java.lang.management.ManagementFactory getRuntimeMXBean getName) 
;;          (take-while (partial not= \@))
;;          (apply str)))
;;   (clj/list
;;    (clj/symbol "defn")
;;    (clj/symbol "pid!")
;;    (clj/vector)
;;    (clj/list
;;     (clj/symbol "->>")
;;     (clj/list
;;      (clj/symbol "..")
;;      (clj/symbol "java.lang.management.ManagementFactory")
;;      (clj/symbol "getRuntimeMXBean")
;;      (clj/symbol "getName"))
;;     (clj/list
;;      (clj/symbol "take-while")
;;      (clj/list
;;       (clj/symbol "partial")
;;       (clj/symbol "not=")
;;       (clj/char "\@")))
;;     (clj/list
;;      (clj/symbol "apply")
;;      (clj/symbol "str"))))
;;   )
