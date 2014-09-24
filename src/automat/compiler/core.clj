(ns automat.compiler.core
  (:refer-clojure :exclude [find])
  (:require
    [potemkin :refer :all]
    [automat.fsm :as fsm])
  (:import
    [automat.fsm
     IAutomaton]))

(definterface+ ICompiledAutomaton
  (start [_ initial-value]
    "Returns a start state for the automaton, with the reduction value set to `initial-value`.")
  (find [_ state stream]
    "Searches for a accepted sub-sequence within the stream.  If an input sequence is rejected, begins again at the automaton's
     start state.  If the input state is `accepted?`, returns immediately.

     If the returned state has `accepted?` set to true, the matching sub-sequence is from `start-index` to `stream-index`. Since
     no inputs past the match are consumed, this can be safely used with impure functions and input sources.")
  (advance-stream [_ state stream reject-value]
    "Advances through the the stream, stopping if the stream is accepted or rejected.  If the input state is `accepted?`,
     proceeds anyway.  If rejected, `reject-value` is returned in lieu of the new state.

     If an input state is already accepted and `reject-value` is returned, this means that inputs beyond the accepted sub-sequence
     have been consumed.  As such, be careful when using impure functions and input sources."))

(defrecord+ CompiledAutomatonState
  [^boolean accepted?
   checkpoint
   ^long state-index
   ^long start-index
   ^long stream-index
   value])

(defn ->automaton-state [fsm x]
  (cond

    (instance? CompiledAutomatonState x)
    x

    (clojure.core/and
      (map? x)
      (contains? x :start-index)
      (contains? x :state-index)
      (contains? x :start-index)
      (contains? x :stream-index)
      (contains? x :checkpoint)
      (contains? x :value))
    (map->CompiledAutomatonState x)

    :else
    (start fsm x)))

(defn normalize-input [x]
  (cond
    (char? x)
    (int x)

    :else
    x))

(defn- parse-input [x]
  (cond
    (clojure.core/or (keyword? x) (number? x)) x
    (char? x) x
    (clojure.core/and (string? x) (= 1 (count x))) (int (first x))
    :else x))

(defn parse-automata [s]
  (let [s (if (sequential? s)
            s
            [s])]
    (if (empty? s)
      (fsm/empty-automaton)
      (->> s
        (cons (fsm/empty-automaton))
        (map
          #(cond
             (instance? IAutomaton %) %
             (vector? %) (parse-automata %)
             :else (fsm/automaton (parse-input %))))
        (apply fsm/concat)
        fsm/minimize))))
