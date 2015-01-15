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

(defn parse-automata
  "Takes either an input, automaton, or sequence of automatons and inputs,
   and returns their concatenation."
  [s]
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

(defn- canonicalize-states [fsm]
  (loop [state->index {}, to-search [(fsm/start fsm)]]
    (if (empty? to-search)
      state->index
      (let [state->index (reduce
                           (fn [m s]
                             (if (contains? m s)
                               m
                               (assoc m s (count m))))
                           state->index
                           to-search)]
        (recur
          state->index
          (->> to-search
            (mapcat #(distinct (vals (fsm/input->state fsm %))))
            (remove #(contains? state->index %))))))))

(defn precompile
  "Takes an fsm, and returns a data structure where states are represented by
   numbers, and the provided keys are :accept, :state->input->state,
   and :state->input->actions.  The start state will always be `0`.

   If the order of actions is important, an `action-comparator` may be defined."
  ([fsm]
     (precompile fsm nil))
  ([fsm action-comparator]
     (let [fsm (-> fsm parse-automata fsm/final-minimize)
           state->index (canonicalize-states fsm)

           accept
           (->> fsm fsm/accept (map state->index) set)

           state->input->state
           (zipmap
             (->> fsm fsm/states (map state->index))
             (->> fsm
               fsm/states
               (map
                 (fn [state]
                   (let [input->state (fsm/input->state fsm state)]
                     (zipmap
                       (keys input->state)
                       (map state->index (vals input->state))))))))

           state->input->actions
           (zipmap
             (->> fsm fsm/states (map state->index))
             (->> fsm
               fsm/states
               (map
                 (fn [state]
                   (let [input->actions (fsm/input->actions fsm state)]
                     (zipmap
                       (keys input->actions)
                       (map
                         (if action-comparator
                           (comp vec (partial sort-by action-comparator))
                           vec)
                         (vals input->actions))))))))]

       {:accept accept
        :state->input->state state->input->state
        :state->input->actions state->input->actions})))

(defn states
  "Returns a list of states for a precompiled automaton"
  [precompiled-fsm]
  (sort
    (distinct
      (concat
        (-> precompiled-fsm :state->input->state keys)
        (->> precompiled-fsm :state->input->state vals (mapcat vals))))))
