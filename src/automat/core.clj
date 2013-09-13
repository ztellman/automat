(ns automat.core
  (:refer-clojure :exclude
    [concat compile find .. * + or and not complement])
  (:use
    [clojure.pprint]
    [potemkin])
  (:require
    [proteus :refer (let-mutable)]
    [automat.fsm :as fsm]
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
    :else x))

(defn parse-automata [s]
  (let [s (if (sequential? s)
            s
            [s])]
    (->> s
      (map
        #(cond
           (instance? IAutomaton %) %
           (vector? %) (parse-automata %)
           :else (fsm/automaton (parse-input %))))
      (apply fsm/concat)
      fsm/minimize)))

(defn $
  [name]
  (fsm/handler-automaton name))

(defn ?
  "Returns an automaton that accepts zero or one of the given automaton."
  [& args]
  (->> args
    parse-automata
    fsm/maybe))

(defn +
  "Returns an automaton that accepts one or more of the given automaton."
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
  "Returns the complement of any zero or one-transition automata.  Equivalent to the character
   negation `^` operator in regular expressions."
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

;;;

(definterface+ ICompiledAutomaton
  (start [_ initial-value]
    "Returns a start state for the automaton, with the reduction value set to `initial-value`.")
  (find [_ state stream]
    "Searches for a accepted sub-sequence within the stream.  If an input sequence is rejected, begins again at the automaton's
     start state.  If the input state is `accepted?`, returns immediately.

     If the returned state has `accepted?` set to true, the matching sub-sequence is from `start-index` to `stream-index`. Since
     no inputs past the match are consumed, this can be safely used with impure functions and input sources.")
  (advance [_ state stream reject-value]
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
    `(let-mutable [state-reduction## (.state ~compiled-state)]
       (loop [state-index## (.state-index ~compiled-state)
              start-index## (.start-index ~compiled-state)
              stream-index## (.stream-index ~compiled-state)]
         (let [input## (~(if numeric? '.nextNumericInput '.nextInput)
                        ~input-stream
                        ~eof-value)]
           (if ~(if numeric?
                  `(== ~eof-value input##)
                  `(identical? ~eof-value input##))
             (if (== stream-index## (.stream-index ~compiled-state))
               ~compiled-state
               (automat.core.CompiledAutomatonState.
                 false
                 (if (.accepted? ~compiled-state)
                   ~compiled-state
                   (.checkpoint ~compiled-state))
                 state-index##
                 start-index##
                 stream-index##
                 state-reduction##))
             (let [stream-index## (p/inc stream-index##)]
               (case state-index##
                 ~@(->> state->index
                     (mapcat
                       (fn [[state index]]
                         `(~index
                            ~(let [input->state (fsm/transitions fsm state)
                                   state->inputs (->> input->state
                                                   keys
                                                   (group-by input->state))]
                               (if (empty? state->inputs)
                                 reject-clause
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
                                                          nil
                                                          ~(state->index state)
                                                          start-index##
                                                          stream-index##
                                                          state-reduction##)
                                                       `(recur ~(state->index state) start-index## stream-index##)))))))))

                                    :else
                                    ~(if-let [default-state (fsm/next-state fsm state fsm/default)]
                                       (if ((fsm/accept fsm) default-state)
                                         `(automat.core.CompiledAutomatonState.
                                            true
                                            nil
                                            ~(state->index default-state)
                                            start-index##
                                            stream-index##
                                            state-reduction##)
                                         `(recur ~(state->index default-state) start-index## stream-index##))
                                       reject-clause))))))))))))))))

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

(defn compile [fsm]
  (if (instance? ICompiledAutomaton fsm)
    fsm
    (let [fsm (fsm/final-minimize (parse-automata fsm))
          state->index (canonicalize-states fsm)]
      (with-meta
        (eval
          (unify-gensyms
            `(reify ICompiledAutomaton
               (start [_# initial-value#]
                 (automat.core.CompiledAutomatonState.
                   ~(contains? (fsm/accept fsm) (fsm/start fsm))
                   nil
                   ~(state->index (fsm/start fsm))
                   0
                   0
                   initial-value#))
               (find [_# state## stream##]
                 (let [stream## (stream/to-stream stream##)]
                   (if (.accepted? ^automat.core.CompiledAutomatonState state##)
                     state##
                     ~(consume-form
                        (with-meta `state## {:tag "automat.core.CompiledAutomatonState"})
                        (with-meta `stream## {:tag "automat.utils.InputStream"})
                        fsm
                        state->index
                        `(recur ~(state->index (fsm/start fsm)) stream-index## stream-index##)))))
               (advance [_# state## stream## reject-value##]
                 (let [stream## (stream/to-stream stream##)]
                   ~(consume-form
                      (with-meta `state## {:tag "automat.core.CompiledAutomatonState"})
                      (with-meta `stream## {:tag "automat.utils.InputStream"})
                      fsm
                      state->index
                      `reject-value##))))))
        {:fsm fsm
         :state->index state->index}))))

(defn greedy-find
  "Greedily find the largest possible accepted sub-sequence.  Will only return an `accepted?` state once subsequent
   inputs have been rejected.  Since this always consumes more inputs than the accepted sub-sequence, be careful when
   using impure functions or input sources."
  [fsm ^CompiledAutomatonState state stream]
  (let [stream (stream/to-stream stream)]
    (loop [state state]
      (if (clojure.core/or (.accepted? state) (.checkpoint state))
        (let [state' (advance fsm state stream ::reject)]
          (cond
            (identical? ::reject state')
            (if (.accepted? state)
              state
              (.checkpoint state))
            
            (identical? state state')
            state

            :else
            (recur state')))
        (let [^CompiledAutomatonState state' (find fsm state stream)]
          (if (.accepted? state')
            (recur state')
            state'))))))
