(ns automat.core
  (:refer-clojure :exclude
    [concat compile find .. * + or and not complement])
  (:use
    [clojure.pprint]
    [potemkin])
  (:require
    [proteus :refer (let-mutable)]
    [automat.fsm :as fsm]
    [automat.viz :as viz]
    [automat.stream :as stream]
    [primitive-math :as p])
  (:import
    [automat.utils
     InputStream]
    [automat.fsm
     IAutomaton]))

;;;

(defn- parse-input [x]
  (cond
    (clojure.core/or (keyword? x) (number? x)) x
    (char? x) (int x)
    (clojure.core/and (string? x) (= 1 (count x))) (int (first x))
    :else nil))

(defn parse-automata [s]
  (let [s (if (sequential? s)
            s
            [s])]
    (->> s
      (map
        #(if (instance? IAutomaton %)
           %
           (if-let [input (parse-input %)]
             (fsm/automaton %)
             (throw
               (IllegalArgumentException.
                 (str "'" (pr-str %) "' is not a valid input."))))))
      (apply fsm/concat)
      fsm/minimize)))

(defn ?
  "Returns an automaton that accepts zero or one of the given automata."
  [& args]
  (->> args
    parse-automata
    fsm/maybe))

(defn +
  "Returns an automaton that accepts one or more of the given automata."
  [& args]
  (let [fsm (parse-automata args)]
    (fsm/concat fsm (fsm/kleene fsm))))

(defn *
  "Returns an automaton that accepts zero or more of the given automata."
  [& args]
  (->> args
    parse-automata
    fsm/kleene))

(defn or
  "Returns an automaton that accepts the union of the given automata."
  [& args]
  (->> args
    (map parse-automata)
    (apply fsm/union)))

(defn and
  "Returns an automaton that accepts the intersection the given automata."
  [& args]
  (->> args
    (map parse-automata)
    (apply fsm/intersection)))

(defn difference
  "Returns an automaton that accepts the disjoint of the first automaton and the subsequent
   automata."
  [& args]
  (->> args
    (map parse-automata)
    (apply fsm/difference)))

(defn complement
  "Returns the complement of the given automaton."
  [& args]
  (->> args
    parse-automata
    fsm/complement))

(defn not
  "Returns the complement of any zero or one-transition automata (i.e. something which accepts
   a single input or none at all)."
  [& args]
  (let [fsm (parse-automata args)]
    (assert (> 3 (count (fsm/states fsm)))
      "'not' can only be used on one or zero-transition automata.")
    (fsm/difference
      (fsm/all-automaton)
      fsm)))

(def ^{:doc "Returns an automaton that accepts any single input value."}
  any (fsm/all-automaton))

(defn- input-range [lower upper]
  (let [lower (parse-input lower)
        upper (parse-input upper)]
    (if (clojure.core/and (number? lower) (number? upper))
      (apply fsm/automaton (range lower (inc upper)))
      (throw
        (IllegalArgumentException.
          (str
            "Don't know how to create a range from '"
            (pr-str lower) "' to '" (pr-str upper) "'"))))))

(defmacro ..
  "Returns an automaton which matches any input within the inclusive range of [upper, lower]."
  [lower upper]
  `(#'automat.core/input-range ~lower ~upper))

(defn fsm [& args]
  (parse-automata args))

;;;

(definterface+ ICompiledAutomaton
  (start [_] "Returns the initial state for the automaton.")
  (find [_ state input-sequence])
  (advance [_ state input-sequence reject-value]))

(defrecord+ CompiledAutomatonState
  [^boolean accepted?
   ^boolean was-accepted?
   ^long state-index
   ^long stream-index
   state])

(defn- inputs-predicate [input to-match]
  (let [non-numbers (remove number? to-match)
        ranges (fsm/input-ranges (filter number? to-match))]
    `(clojure.core/or
       ~@(map
           (fn [i]
             (if (keyword? i)
               `(identical? ~input ~i)
               `(= ~input ~i)))
           non-numbers)
       ~@(map
           (fn [[l u]]
             (if (== l u)
               `(== ~input ~l)
               `(clojure.core/and (<= ~l ~input) (<= ~input ~u))))
           ranges))))

(defn- consume-form
  [compiled-state input-stream fsm state->index reject-clause]
  (let [numeric? (every? number? (fsm/alphabet fsm))
        eof-value (if numeric? Integer/MIN_VALUE ::eof)]
    `(let [was-accepted?## (.was-accepted? ~compiled-state)]
       (let-mutable [state-reduction## (.state ~compiled-state)
                     stream-index## (.stream-index ~compiled-state)]
         (loop [state-index## (.state-index ~compiled-state)]
           (let [input## (~(if numeric? '.nextNumericInput '.nextInput)
                          ~input-stream
                          ~eof-value)]
             (if ~(if numeric?
                    `(== ~eof-value input##)
                    `(identical? ~eof-value input##))
               (if (== stream-index## (.stream-index ~compiled-state))
                 ~compiled-state
                 (automat.core.CompiledAutomatonState.
                   (.accepted? ~compiled-state)
                   was-accepted?##
                   state-index##
                   stream-index##
                   state-reduction##))
               (do
                 (set! stream-index## (p/inc (unchecked-long stream-index##)))
                 (case state-index##
                   ~@(->> state->index
                       (mapcat
                         (fn [[state index]]
                           `(~index
                              ~(let [input->state (fsm/transitions fsm state)
                                     state->inputs (->> input->state
                                                     keys
                                                     (group-by input->state))]
                                 (when (clojure.core/not (empty? state->inputs))
                                   `(cond
                                      ~@(->> state->inputs
                                          (mapcat
                                            (fn [[state inputs]]
                                              (let [inputs (remove #{fsm/default} inputs)]
                                                (when (clojure.core/not (empty? inputs))
                                                  `(~(inputs-predicate `input## inputs)
                                                    ~(if (= fsm/reject state)
                                                       reject-clause
                                                       (if ((fsm/accept fsm) state)
                                                         `(automat.core.CompiledAutomatonState.
                                                            true
                                                            true
                                                            ~(state->index state)
                                                            stream-index##
                                                            state-reduction##)
                                                         `(recur ~(state->index state))))))))))
                                      :else
                                      ~(if-let [default-state (fsm/next-state fsm state fsm/default)]
                                         (if ((fsm/accept fsm) default-state)
                                           `(automat.core.CompiledAutomatonState.
                                              true
                                              true
                                              ~(state->index default-state)
                                              stream-index##
                                              state-reduction##)
                                           `(recur ~(state->index default-state)))
                                         reject-clause)))))))))))))))))

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
            (mapcat #(vals (fsm/transitions fsm %)))
            (remove #(contains? state->index %))))))))

(defn compile [& args]
  (if (clojure.core/and (= 1 (count args))
        (instance? ICompiledAutomaton (first args)))
    (first args)
    (let [fsm (fsm/minimize (parse-automata args))
          state->index (canonicalize-states fsm)]
      (with-meta
        (eval
          (unify-gensyms
            `(reify ICompiledAutomaton
               (start [_#]
                 (automat.core.CompiledAutomatonState.
                   ~(contains? (fsm/accept fsm) (fsm/start fsm))
                   false
                   ~(state->index (fsm/start fsm))
                   0
                   nil))
               (find [_# state## input-stream##]
                 (if (.accepted? ^automat.core.CompiledAutomatonState state##)
                   state##
                   ~(consume-form
                      (with-meta `state## {:tag "automat.core.CompiledAutomatonState"})
                      (with-meta `input-stream## {:tag "automat.utils.InputStream"})
                      fsm
                      state->index
                      `(recur ~(state->index (fsm/start fsm))))))
               (advance [_# state## input-stream## reject-value##]
                 ~(consume-form
                    (with-meta `state## {:tag "automat.core.CompiledAutomatonState"})
                    (with-meta `input-stream## {:tag "automat.utils.InputStream"})
                    fsm
                    state->index
                    `reject-value##)))))
       {:fsm fsm
        :state->index state->index}))))

(defn greedy-find [fsm ^CompiledAutomatonState state input-sequence]
  (loop [state state]
    (if (.was-accepted? state)
      (let [state' (advance fsm state input-sequence ::reject)]
        (if (or
              (identical? ::reject state')
              (identical? state state'))
          state
          (recur state')))
      (recur (find fsm state input-sequence)))))

(defn view [compiled-fsm]
  (if-not (instance? ICompiledAutomaton compiled-fsm)
    (IllegalArgumentException. "Can only visualize a compiled FSM.")
    (let [fsm (-> compiled-fsm meta :fsm)
          state->index (-> compiled-fsm meta :state->index)]
      (viz/view-fsm fsm state->index))))
