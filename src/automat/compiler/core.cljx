(ns automat.compiler.core
  (:refer-clojure :exclude [find])
  (:require
   [automat.fsm :as fsm :refer [#+cljs IAutomaton]]
   #+clj [potemkin :refer :all])
  #+clj
  (:import
   [automat.fsm
    IAutomaton]))

(#+clj definterface+ #+cljs defprotocol ICompiledAutomaton
  (start [_ initial-value]
    "Returns a start state for the automaton, with the reduction value set to `initial-value`.")
  (find [_ state stream])
  (advance-stream [_ state stream reject-value]))

(defn compiled-automaton? [x]
  (#+clj instance? #+cljs satisfies? ICompiledAutomaton x))

(#+clj defrecord+ #+cljs defrecord CompiledAutomatonState
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

    (and
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
             (fsm/automaton? %) %
             (vector? %) (parse-automata %)
             :else (fsm/automaton %)))
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

(defn precompiled-automaton? [fsm]
  (and (map? fsm)
       (contains? fsm :accept)
       (contains? fsm :state->input->actions)
       (contains? fsm :state->input->state)))

(defn precompile
  "Takes an fsm, and returns a data structure where states are represented by
  numbers, and the provided keys are :accept, :state->input->state,
  and :state->input->actions.  The start state will always be `0`."
  [fsm]
  (if (precompiled-automaton? fsm)
    fsm
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
                      (map set (vals input->actions))))))))
          precompiled-fsm {:accept accept
                           :state->input->actions state->input->actions
                           :state->input->state state->input->state}]
      (with-meta precompiled-fsm {:fsm fsm}))))

(defn sort-actions [fsm comparator]
  {:pre [(precompiled-automaton? fsm)]}
  (->> fsm
       :state->input->actions
       (reduce-kv
        (fn [state->input->actions state input->actions]
          (->> input->actions
               (reduce-kv
                (fn [input->actions input actions]
                  (assoc! input->actions input (sort comparator actions)))
                (transient {}))
               persistent!
               (assoc! state->input->actions state)))
        (transient {}))
       persistent!
       (assoc fsm :state->input->actions)))

(defn states
  "Returns a list of states for a precompiled automaton"
  [fsm]
  {:pre [(precompiled-automaton? fsm)]}
  (sort
    (distinct
      (concat
        (-> fsm :state->input->state keys)
        (->> fsm :state->input->state vals (mapcat vals))))))
