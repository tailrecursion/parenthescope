(ns tailrecursion.parenthescope.printers)

(defprotocol PagingBuffer
  (object [_ point] "Index of the top-most object at point.")
  (bounds [_ object] "Pair of the start and end points of object, inclusive.")
  (page [_ start max] "PagingBuffer containing all fully-printed objects from start up to max, inclusive."))

(def none ::none)
