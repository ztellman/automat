(ns automat.core
  (:refer-clojure :exclude
    [concat compile find .. * + or and not complement])
  (:use
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

(defn $
  "Defines a state tag, which can be correlated to a reducer function using `compile`."
  [name]
  (fsm/action-automaton name))

(defn interpose-$
  "Applies a state tag to all states within the given automaton."
  [name fsm]
  (-> fsm
    parse-automata
    (fsm/add-action name)))

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

(defn- inputs-predicate [input to-match]
  (let [non-numbers (remove number? to-match)
        ranges (fsm/input-ranges (filter number? to-match))]
    `(clojure.core/or
       ~@(map
           (fn [i]
             (if (keyword? i)
               `(identical? ~input ~i)
               `(= ~input '~i)))
           non-numbers)
       ~@(map
           (fn [[l u]]
             (if (== l u)
               `(== ~input ~l)
               `(clojure.core/and (<= ~l ~input) (<= ~input ~u))))
           ranges))))

(defn- consume-form
  [compiled-state input-stream fsm state->index reject-clause signal action->sym]
  (let [alphabet (disj (fsm/alphabet fsm) fsm/default)
        numeric? (clojure.core/and
                   (clojure.core/not (empty? alphabet))
                   (every? number? alphabet))
        eof-value (if numeric? Integer/MIN_VALUE ::eof)
        start-index (state->index (fsm/start fsm))
        next-input `(~(if numeric? '.nextNumericInput '.nextInput)
                     ~input-stream
                     ~eof-value)]
    `(let-mutable [value##   (.value ~compiled-state)]
       (loop [state-index##  (.state-index ~compiled-state)
              start-index##  (.start-index ~compiled-state)
              stream-index## (.stream-index ~compiled-state)
              input##        ~next-input]
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
               value##))
           (let [signal## ~(if signal `(~signal input##) `input##)
                 stream-index## (p/inc stream-index##)]
             (case state-index##
               ~@(->> state->index
                   (sort-by val)
                   (mapcat
                     (fn [[state index]]
                       `(~index
                          ~(let [input->state (fsm/input->state fsm state)
                                 input->actions (fsm/input->actions fsm state)

                                 state+actions->inputs
                                 (->> input->state
                                   keys
                                   (group-by
                                     (juxt
                                       (clojure.core/or input->state {})
                                       (clojure.core/or input->actions {}))))]

                             (if (empty? state+actions->inputs)
                               (reject-clause next-input)
                               `(do
                                  ~@(when-let [action-syms (->> state
                                                             (fsm/input->actions fsm)
                                                             (#(get % fsm/pre))
                                                             (map action->sym)
                                                             (remove nil?)
                                                             seq)]
                                      `((set! value##
                                          ~(reduce
                                             (fn [form fn] `(~fn ~form input##))
                                             `value##
                                             action-syms))))
                                  (cond
                                    ~@(->> state+actions->inputs
                                        (mapcat
                                          (fn [[[state actions] inputs]]
                                            (let [inputs (remove #{fsm/default} inputs)]
                                              (when (clojure.core/not (empty? inputs))
                                                `(~(inputs-predicate `signal## inputs)
                                                  (do

                                                    ;; update value
                                                    ~@(when-let [action-syms (->> actions
                                                                               (map action->sym)
                                                                               (remove nil?)
                                                                               seq)]
                                                        `((set! value##
                                                            ~(reduce
                                                               (fn [form fn] `(~fn ~form input##))
                                                               `value##
                                                               action-syms))))

                                                    ;; recur to next state, or return accepted state
                                                    ~(if (= fsm/reject state)
                                                       (reject-clause next-input)
                                                       (if ((fsm/accept fsm) state)
                                                         `(automat.core.CompiledAutomatonState.
                                                            true
                                                            nil
                                                            ~(state->index state)
                                                            start-index##
                                                            stream-index##
                                                            value##)
                                                         `(recur ~(state->index state) start-index## stream-index## ~next-input))))))))))

                                    :else
                                    ~(if-let [default-state (fsm/next-state fsm state fsm/default)]
                                       (if ((fsm/accept fsm) default-state)
                                         `(automat.core.CompiledAutomatonState.
                                            true
                                            nil
                                            ~(state->index default-state)
                                            start-index##
                                            stream-index##
                                            value##)
                                         `(recur ~(state->index default-state) start-index## stream-index## ~next-input))
                                       (reject-clause next-input)))))))))))))))))

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
            (mapcat #(vals (fsm/input->state fsm %)))
            (remove #(contains? state->index %))))))))

(def ^:dynamic *fns*)

(defn compile
  "Compiles the fsm into something that can be used with `find`, `advance-stream`, `greedy-find`, and `advance`.  Optionally takes an option map, which may contain:

    `reducers` - a map or function of actions defined via `$` onto reducer functions which take two arguments, the current reduction value and the input.

    `signal` - a function that takes an input and returns the signal that will be used to advance the automaton.  The input passed into reducer functions will not be affected by this."
  ([fsm]
     (compile fsm nil))
  ([fsm {:keys [reducers signal]}]
     (if (instance? ICompiledAutomaton fsm)
       fsm
       (let [fsm (fsm/final-minimize (parse-automata fsm))
             state->index (canonicalize-states fsm)
             action->sym (zipmap
                           (->> fsm
                             fsm/states
                             (mapcat #(vals (fsm/input->actions fsm %)))
                             (remove nil?)
                             (apply clojure.core/concat)
                             distinct)
                            (repeatedly #(gensym "f")))
             action->fn (->> action->sym
                           keys
                           (map #(when-let [f (clojure.core/and reducers (reducers %))]
                                   [% f]))
                           (remove nil?)
                           (into {}))
             action->sym (select-keys action->sym (keys action->fn))]
         (with-meta
           (binding [*fns* action->fn]
             (eval
               (unify-gensyms
                 `(let [~@(mapcat
                            (fn [[action sym]]
                              [sym `(*fns* ~action)])
                            action->sym)]
                    (reify automat.core.ICompiledAutomaton
                      (start [_# initial-value#]
                        (automat.core.CompiledAutomatonState.
                          ~(contains? (fsm/accept fsm) (fsm/start fsm))
                          nil
                          ~(state->index (fsm/start fsm))
                          0
                          0
                          initial-value#))
                      (find [_# state## stream##]
                        (let [state## (if (instance? automat.core.CompiledAutomatonState state##)
                                        state##
                                        (map->CompiledAutomatonState state##))
                              stream## (stream/to-stream stream##)]
                          (if (.accepted? ^automat.core.CompiledAutomatonState state##)
                            state##
                            ~(consume-form
                               (with-meta `state## {:tag "automat.core.CompiledAutomatonState"})
                               (with-meta `stream## {:tag "automat.utils.InputStream"})
                               fsm
                               state->index
                               (fn [next-input]
                                 (let [start-index (state->index (fsm/start fsm))]
                                   `(if (p/== ~start-index state-index##)
                                      (recur ~start-index stream-index## stream-index## ~next-input)
                                      (recur ~start-index (p/dec stream-index##) (p/dec stream-index##) input##))))
                               signal
                               action->sym))))
                      (advance-stream [_# state## stream## reject-value##]
                        (let [state## (if (instance? automat.core.CompiledAutomatonState state##)
                                        state##
                                        (map->CompiledAutomatonState state##))
                              stream## (stream/to-stream stream##)]
                          ~(consume-form
                             (with-meta `state## {:tag "automat.core.CompiledAutomatonState"})
                             (with-meta `stream## {:tag "automat.utils.InputStream"})
                             fsm
                             state->index
                             (fn [_] `reject-value##)
                             signal
                             action->sym))))))))
           {:fsm fsm
            :state->index state->index})))))

(defn greedy-find
  "Greedily find the largest possible accepted sub-sequence.  Will only return an `accepted?` state once subsequent
   inputs have been rejected.  Since this always consumes more inputs than the accepted sub-sequence, be careful when
   using impure functions or input sources."
  [fsm ^CompiledAutomatonState state stream]
  (let [stream (stream/to-stream stream)]
    (loop [state state]
      (if (clojure.core/or (.accepted? state) (.checkpoint state))
        (let [state' (advance-stream fsm state stream ::reject)]
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

(defn advance
  "Advances a single position in the automaton.  Takes a compiled `fsm`, a `state` which is either an initial reduce
   value or a CompiledAutomatonState previously returned by `advance`, and an input.

   If a `reject-value` is specified, it will be returned if an invalid input is given.  Otherwise an IllegalArgumentException is thrown."
  ([fsm state input]
     (let [state' (advance fsm state input ::reject)]
       (if (identical? ::reject state')
         (throw
           (IllegalArgumentException.
             (str "could not process input " (pr-str input))))
         state')))
  ([fsm state input reject-value]
     (let [state (if (instance? CompiledAutomatonState state)
                   state
                   (start fsm state))]
       (advance-stream fsm state [input] reject-value))))
