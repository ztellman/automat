(ns automat.core
  (:refer-clojure :exclude [concat compile find range .. * + or and not complement])
  (:require
   [automat.compiler.base :as base]
   [automat.compiler.core :as core :refer [#+cljs CompiledAutomatonState]]
   [automat.fsm :as fsm]
   [automat.stream :as stream]
   [clojure.set :as set]
   #+clj [automat.compiler.eval :as eval]
   #+clj [potemkin]
   #+clj [clojure.core :as clj]
   #+cljs [cljs.core :as clj :include-macros true])
  #+clj
  (:import
   [automat.compiler.core
    CompiledAutomatonState]))

;;;

(defn $
  "Defines a state tag, which can be correlated to a reducer function using `compile`."
  [tag]
  {:pre [(keyword? tag)]}
  (fsm/action-automaton tag))

(defn interpose-$
  "Applies a state tag to all states within the given automaton."
  [tag fsm]
  {:pre [(keyword? tag)]}
  (-> fsm
    core/parse-automata
    (fsm/add-action tag)))

(defn ?
  "Returns an automaton that accepts zero or one of the given automaton."
  [& args]
  (->> args
    core/parse-automata
    fsm/maybe))

(defn +
  "Returns an automaton that accepts one or more of the given automaton."
  [& args]
  (let [fsm (core/parse-automata args)]
    (fsm/concat fsm (fsm/kleene fsm))))

(defn *
  "Returns an automaton that accepts zero or more of the given automata."
  [& args]
  (->> args
    core/parse-automata
    fsm/kleene))

(defn range
  "Returns an automaton which matches any input within the inclusive range of [upper, lower]."
  [lower upper]
  (if (clj/and (number? lower) (number? upper))
    (apply fsm/automaton (clj/range lower (inc upper)))
    (throw
     (#+clj IllegalArgumentException. #+cljs js/Error.
      (str
       "Don't know how to create a range from '"
       (pr-str lower) "' to '" (pr-str upper) "'")))))

;;;

(defn matching-inputs
  "Returns a lazy sequence of input sequences which the automaton will match."
  [fsm]
  (fsm/matching-inputs (core/parse-automata fsm)))

(defn find
  "Searches for a accepted sub-sequence within the stream.  If an input sequence is rejected, begins again at the automaton's
  start state. If the input state is `accepted?`, returns immediately.

  If the returned state has `accepted?` set to true, the matching sub-sequence is from `start-index` to `stream-index`. Since
  no inputs past the match are consumed, this can be safely used with impure functions and input sources."
  [fsm state stream]
  (core/find fsm state stream))

(defn advance-stream
  "Advances through the the stream, stopping if the stream is accepted or rejected.  If the input state is `accepted?`,
  proceeds anyway.  If rejected, `reject-value` is returned in lieu of the new state.

  If an input state is already accepted and `reject-value` is returned, this means that inputs beyond the accepted sub-sequence
  have been consumed.  As such, be careful when using impure functions and input sources."
  [fsm state stream reject-value]
  (core/advance-stream fsm state stream reject-value))

(defn greedy-find
  "Greedily find the largest possible accepted sub-sequence.  Will only return an `accepted?` state once subsequent
   inputs have been rejected.  Since this always consumes more inputs than the accepted sub-sequence, be careful when
   using impure functions or input sources."
  [fsm state stream]
  (let [^CompiledAutomatonState state (core/->automaton-state fsm state)
        stream (stream/to-stream stream)]
    (loop [state state]
      (if (clj/or (.-accepted? state) (.-checkpoint state))
        (let [state' (advance-stream fsm state stream ::reject)]
          (cond
            (#+clj identical? #+cljs keyword-identical? ::reject state')
            (if (.-accepted? state)
              state
              (.-checkpoint state))

            (identical? state state')
            state

            :else
            (recur state')))
        (let [^CompiledAutomatonState state' (find fsm state stream)]
          (if (.-accepted? state')
            (recur state')
            state'))))))

(defn advance
  "Advances a single position in the automaton.  Takes a compiled `fsm`, a `state` which is either an initial reduce
   value or a CompiledAutomatonState previously returned by `advance`, and an input.

   If a `reject-value` is specified, it will be returned if an invalid input is given.  Otherwise an IllegalArgumentException is thrown."
  ([fsm state input]
     (let [state' (advance fsm state input ::reject)]
       (if (#+clj identical? #+cljs keyword-identical? ::reject state')
         (throw
           (#+clj IllegalArgumentException. #+cljs js/Error.
             (str "could not process input " (pr-str input))))
         state')))
  ([fsm state input reject-value]
     (let [state (core/->automaton-state fsm state)]
       (advance-stream fsm state [input] reject-value))))

#+clj (potemkin/import-fn core/precompile)
#+cljs (def precompile core/precompile) ; TODO: port potemkin to cljs ;)

(defn compile
  "Compiles the fsm into something that can be used with `find`, `advance-stream`, `greedy-find`, and `advance`.  Optionally takes an option map, which may contain:

    `reducers` - a map or function of actions defined via `$` onto reducer functions which take two arguments, the current reduction value and the input.

    `signal` - a function that takes an input and returns the signal that will be used to advance the automaton.  The input passed into reducer functions will not be affected by this.

    `action-comparator` - an optional comparator function used to sort actions. The default is `compare`."
  ([fsm]
   (compile fsm nil))
  ([fsm {:keys [backend action-comparator] :as options}]
   (if (core/compiled-automaton? fsm)
     fsm
     (let [fsm (core/precompile fsm)
           fsm (if action-comparator
                 (core/sort-actions fsm action-comparator)
                 fsm)
           backend (clj/or
                    backend
                    #+clj (when (-> fsm core/states count (< 30)) :eval)
                    :base)]
       ((case backend
          #+clj #+clj
          :eval eval/compile
          :base base/compile)
        fsm options)))))

;;; define these last, so we don't use them mistakenly elsewhere

(defn or
  "Returns an automaton that accepts the union of the given automata."
  [& args]
  (->> args
    (map core/parse-automata)
    (apply fsm/union)
    fsm/minimize))

(defn and
  "Returns an automaton that accepts the intersection the given automata."
  [& args]
  (->> args
    (map core/parse-automata)
    (apply fsm/intersection)
    fsm/minimize))

(defn difference
  "Returns an automaton that accepts the disjoint of the first automaton and the subsequent
   automata."
  [& args]
  (->> args
    (map core/parse-automata)
    (apply fsm/difference)
    fsm/minimize))

(defn complement
  "Returns the complement of the given automaton."
  [& args]
  (->> args
    core/parse-automata
    fsm/complement))

(defn not
  "Returns the complement of any zero or one-transition automata.  Equivalent to the character
   negation `^` operator in regular expressions."
  [& args]
  (let [fsm (core/parse-automata args)]
    (assert (> 3 (count (fsm/states fsm)))
      "'not' can only be used on one or zero-transition automata.")
    (fsm/difference
      (fsm/all-automaton)
      fsm)))

(def ^{:doc "Returns an automaton that accepts any single input value."}
  any (fsm/all-automaton))

(def ^{:doc "Returns an automaton that immediately accepts."}
  none (fsm/empty-automaton))
