(ns automat.compiler.eval
  (:refer-clojure :exclude [find compile])
  (:require
    [proteus]
    [primitive-math :as p]
    [potemkin :refer :all]
    [automat.fsm :as fsm]
    [automat.compiler.core :refer :all])
  (:import
    [automat.compiler.core
     ICompiledAutomaton
     CompiledAutomatonState]))

(defn- inputs-predicate [numeric? input to-match]
  (let [to-match (map normalize-input to-match)
        non-numbers (remove number? to-match)
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
             (let [x (if (== l u)
                       `(== ~input ~l)
                       `(clojure.core/and (<= ~l ~input) (<= ~input ~u)))]
               (if numeric?
                 x
                 `(clojure.core/and (number? ~input) ~x))))
           ranges))))

(defn- consume-form
  [compiled-state input-stream fsm state->index reject-clause signal action->sym]
  (let [alphabet (disj (fsm/alphabet fsm) fsm/default)
        numeric? (clojure.core/and
                   (clojure.core/not (empty? alphabet))
                   (->> alphabet (map normalize-input) (every? number?)))
        eof-value (if numeric? Integer/MIN_VALUE ::eof)
        start-index (state->index (fsm/start fsm))
        next-input `(~(if numeric? '.nextNumericInput '.nextInput)
                     ~input-stream
                     ~eof-value)]
    `(proteus/let-mutable [value##   (.value ~compiled-state)]
       (loop [state-index##  (.state-index ~compiled-state)
              start-index##  (.start-index ~compiled-state)
              stream-index## (.stream-index ~compiled-state)
              input##        ~next-input]

         (if ~(if numeric?
                `(== ~eof-value input##)
                `(identical? ~eof-value input##))

           ;; end of the line, return the state
           (if (== stream-index## (.stream-index ~compiled-state))
             ~compiled-state
             (automat.compiler.core.CompiledAutomatonState.
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
                          ~(let [input->state (clojure.core/or (fsm/input->state fsm state) {})

                                 state+actions->inputs
                                 (->> input->state
                                   keys
                                   (group-by (juxt input->state #(fsm/actions fsm state %))))]

                             (if (empty? state+actions->inputs)
                               (reject-clause next-input)
                               `(do

                                  ;; do any pre-actions first
                                  ~@(when-let [action-syms (->> (fsm/actions fsm state fsm/pre)
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
                                                `(~(inputs-predicate numeric? `signal## inputs)
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
                                                         `(automat.compiler.core.CompiledAutomatonState.
                                                            true
                                                            nil
                                                            ~(state->index state)
                                                            start-index##
                                                            stream-index##
                                                            value##)
                                                         `(recur ~(state->index state) start-index## stream-index## ~next-input))))))))))

                                    ;; default value
                                    :else
                                    ~(if-let [default-state (fsm/next-state fsm state fsm/default)]
                                       `(do

                                          ~@(when-let [action-syms (->> (fsm/actions fsm state fsm/default)
                                                                     (map action->sym)
                                                                     (remove nil?)
                                                                     seq)]
                                              `((set! value##
                                                  ~(reduce
                                                     (fn [form fn] `(~fn ~form input##))
                                                     `value##
                                                     action-syms))))

                                          ~(if ((fsm/accept fsm) default-state)
                                             `(automat.compiler.core.CompiledAutomatonState.
                                                true
                                                nil
                                                ~(state->index default-state)
                                                start-index##
                                                stream-index##
                                                value##)
                                             `(recur ~(state->index default-state) start-index## stream-index## ~next-input)))
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
            (mapcat #(distinct (vals (fsm/input->state fsm %))))
            (remove #(contains? state->index %))))))))

(def ^:dynamic *fns*)
(def ^:dynamic *signal*)

(defn compile
  [fsm {:keys [reducers signal]}]
  (if (instance? ICompiledAutomaton fsm)
    fsm
    (let [fsm (fsm/final-minimize (parse-automata fsm))
          state->index (canonicalize-states fsm)
          action->sym (zipmap
                        (->> fsm
                          fsm/states
                          (mapcat #(vals (fsm/input->actions fsm %)))
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
        (binding [*fns* action->fn
                  *signal* signal]
          (eval
            (unify-gensyms
              `(let [~@(mapcat
                         (fn [[action sym]]
                           [sym `(*fns* ~action)])
                         action->sym)
                     signal## @#'*signal*]
                 (reify automat.compiler.core.ICompiledAutomaton
                   (start [_# initial-value#]
                     (automat.compiler.core.CompiledAutomatonState.
                       ~(contains? (fsm/accept fsm) (fsm/start fsm))
                       nil
                       ~(state->index (fsm/start fsm))
                       0
                       0
                       initial-value#))
                   (find [this# state## stream##]
                     (let [state## (->automaton-state this# state##)
                           stream## (automat.stream/to-stream stream##)]
                       (if (.accepted? ^automat.compiler.core.CompiledAutomatonState state##)
                         state##
                         ~(consume-form
                            (with-meta `state## {:tag "automat.compiler.core.CompiledAutomatonState"})
                            (with-meta `stream## {:tag "automat.utils.InputStream"})
                            fsm
                            state->index
                            (fn [next-input]
                              (let [start-index (state->index (fsm/start fsm))]
                                `(if (p/== ~start-index state-index##)
                                   (recur ~start-index stream-index## stream-index## ~next-input)
                                   (recur ~start-index (p/dec stream-index##) (p/dec stream-index##) input##))))
                            (when signal `signal##)
                            action->sym))))
                   (advance-stream [this# state## stream## reject-value##]
                     (let [state## (->automaton-state this# state##)
                           stream## (automat.stream/to-stream stream##)]
                       ~(consume-form
                          (with-meta `state## {:tag "automat.compiler.core.CompiledAutomatonState"})
                          (with-meta `stream## {:tag "automat.utils.InputStream"})
                          fsm
                          state->index
                          (fn [_] `reject-value##)
                          (when signal `signal##)
                          action->sym))))))))
        {:fsm fsm
         :state->index state->index}))))
