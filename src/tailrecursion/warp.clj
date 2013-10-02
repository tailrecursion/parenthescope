(ns tailrecursion.warp
  "Conditionals in the exception dimension enabling faster-than-logic (FTL) travel.  Based on the Alcubierre drive."
  (:import java.util.WeakHashMap)
  (:refer-clojure :exclude [cond or]))

(def ^:dynamic *e*)

(def ^:private thrown (atom (WeakHashMap.)))

(defmacro throw! [expr]
  `(let [t# ~expr]
     (swap! @#'thrown #(doto % (.put t# true)))
     (throw t#)))

(defmacro if*
  ([test then]
     `(if* ~test ~then nil))
  ([test then else?]
     `(try ~test ~then
           (catch Throwable t#
              (if (.get @@#'thrown t#)
               (throw t#)
               (binding [*e* t#] ~else?))))))

(defmacro cond
  [& clauses]
  (when clauses
    (list 'tailrecursion.warp/if* (first clauses)
          (if (next clauses)
            (second clauses)
            (throw (IllegalArgumentException.
                    "cond requires an even number of forms")))
          (cons 'tailrecursion.warp/cond (next (next clauses))))))

(defmacro or
  ([] nil)
  ([x] `(try ~x
             (catch Throwable t#
               (if (.get @@#'thrown t#) (throw t#)))))
  ([x & next]
     `(try ~x (catch Throwable t#
                (if (.get @@#'thrown t#)
                  (throw t#)
                  (binding [*e* t#] (or ~@next)))))))

(comment
  (require '[tailrecursion.warp :as ! :refer [throw! *e*]])
  (!/if* (throw (Exception. "uhoh")) 1 (str "exception: " *e*)) ;=> "exception: java.lang.Exception: uhoh"
  (!/or (throw (Exception. "uhoh"))) ;=> nil
  (!/or (throw! (Exception. "uhoh"))) ;=> Exception uhoh  user/eval294 (NO_SOURCE_FILE:30)
  (!/or (nth [] -1) *e*) ;=> #<IndexOutOfBoundsException java.lang.IndexOutOfBoundsException>
  (!/cond (throw (Exception. "uhoh")) :never :else 123) ;=> 123
  )
