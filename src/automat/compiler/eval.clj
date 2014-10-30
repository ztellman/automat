(ns automat.compiler.eval
  (:refer-clojure :exclude [find compile])
  (:require
    [proteus]
    [primitive-math :as p]
    [potemkin :refer :all]
    [automat.fsm :as fsm]
    [automat.compiler.core :as core])
  (:import
    [automat.compiler.core
     ICompiledAutomaton
     CompiledAutomatonState]))

(defn- inputs-predicate [numeric? input to-match]
  (let [to-match (map core/normalize-input to-match)
        non-numbers (remove number? to-match)
        ranges (fsm/input-ranges (filter number? to-match))]
    `(or
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
                       `(and (<= ~l ~input) (<= ~input ~u)))]
               (if numeric?
                 x
                 `(and (number? ~input) ~x))))
           ranges))))

(defn- consume-form
  [compiled-state input-stream fsm reject-clause signal action->sym]
  (let [alphabet (disj
                   (->>
                     fsm
                     :state->input->state
                     vals
                     (mapcat keys)
                     set)
                   fsm/default)
        states (core/states fsm)
        numeric? (and
                   (not (empty? alphabet))
                   (->> alphabet (map core/normalize-input) (every? number?)))
        eof-value (if numeric? Integer/MIN_VALUE ::eof)
        next-input `(~(if numeric? '.nextNumericInput '.nextInput)
                     ~input-stream
                     ~eof-value)]
    `(proteus/let-mutable [value## (.value ~compiled-state)]
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
               ~@(mapcat
                   (fn [state]
                     `(~state
                        ~(let [input->state (-> fsm :state->input->state (get state))

                               state+actions->inputs
                               (->> input->state
                                 keys
                                 (group-by (juxt input->state #(get-in fsm [:state->input->actions state %]))))]

                           (if (empty? state+actions->inputs)
                             (reject-clause next-input)
                             `(do

                                ;; do any pre-actions first
                                ~@(when-let [action-syms (->> (get-in fsm [:state->input->actions state fsm/pre])
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
                                            (when (not (empty? inputs))
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
                                                     (if (contains? (:accept fsm) state)
                                                       `(automat.compiler.core.CompiledAutomatonState.
                                                          true
                                                          nil
                                                          ~state
                                                          start-index##
                                                          stream-index##
                                                          value##)
                                                       `(recur ~state start-index## stream-index## ~next-input))))))))))

                                  ;; default value
                                  :else
                                  ~(if-let [default-state (get-in fsm [:state->input->state state fsm/default])]
                                     `(do

                                        ~@(when-let [action-syms (->> (get-in fsm [:state->input->actions state fsm/default])
                                                                   (map action->sym)
                                                                   (remove nil?)
                                                                   seq)]
                                            `((set! value##
                                                ~(reduce
                                                   (fn [form fn] `(~fn ~form input##))
                                                   `value##
                                                   action-syms))))

                                        ~(if (contains? (:accept fsm) default-state)
                                           `(automat.compiler.core.CompiledAutomatonState.
                                              true
                                              nil
                                              ~default-state
                                              start-index##
                                              stream-index##
                                              value##)
                                           `(recur ~default-state start-index## stream-index## ~next-input)))
                                     (reject-clause next-input))))))))
                   states))))))))

(def ^:dynamic *fns*)
(def ^:dynamic *signal*)

(defn compile
  [fsm {:keys [reducers signal]}]
  (if (instance? ICompiledAutomaton fsm)
    fsm
    (let [base-fsm fsm
          fsm (core/precompile fsm)
          action->sym (zipmap
                        (->> fsm
                          :state->input->actions
                          vals
                          (mapcat vals)
                          (apply concat)
                          distinct)
                        (repeatedly #(gensym "f")))
          action->fn (->> action->sym
                       keys
                       (map #(when-let [f (and reducers (reducers %))]
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
                       ~(contains? (:accept fsm) 0)
                       nil
                       0
                       0
                       0
                       initial-value#))
                   (find [this# state## stream##]
                     (let [state## (core/->automaton-state this# state##)
                           stream## (automat.stream/to-stream stream##)]
                       (if (.accepted? ^automat.compiler.core.CompiledAutomatonState state##)
                         state##
                         ~(consume-form
                            (with-meta `state## {:tag "automat.compiler.core.CompiledAutomatonState"})
                            (with-meta `stream## {:tag "automat.utils.InputStream"})
                            fsm
                            (fn [next-input]
                              `(if (p/== 0 state-index##)
                                 (recur 0 stream-index## stream-index## ~next-input)
                                 (recur 0 (p/dec stream-index##) (p/dec stream-index##) input##)))
                            (when signal `signal##)
                            action->sym))))
                   (advance-stream [this# state## stream## reject-value##]
                     (let [state## (core/->automaton-state this# state##)
                           stream## (automat.stream/to-stream stream##)]
                       ~(consume-form
                          (with-meta `state## {:tag "automat.compiler.core.CompiledAutomatonState"})
                          (with-meta `stream## {:tag "automat.utils.InputStream"})
                          fsm
                          (fn [_] `reject-value##)
                          (when signal `signal##)
                          action->sym))))))))
        {:fsm base-fsm}))))
