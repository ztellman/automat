(ns automat.compiler.base
  (:refer-clojure :exclude [compile])
  (:require
    [automat.compiler.core :as core]
    [automat.fsm :as fsm]
    [automat.stream :as stream])
  (:import
    [automat.compiler.core
     ICompiledAutomaton
     CompiledAutomatonState]
    [automat.fsm
     IAutomaton]))

(defn advance [fsm state stream signal reducers restart?]
  (let [^CompiledAutomatonState state state
        ^automat.utils.InputStream stream (stream/to-stream stream)
        start-index (.start-index state)
        stream-index (.stream-index state)]
    (loop [input (signal (.nextInput stream ::eof))
           value (.value state)
           state (.state-index state)
           start-index start-index
           stream-index stream-index]

      (if (identical? ::eof input)

        (CompiledAutomatonState.
          (boolean (contains? (:accept fsm) state))
          nil
          state
          start-index
          stream-index
          value)

        (if-let [state' (or
                          (get-in fsm [:state->input->state state input])
                          (get-in fsm [:state->input->state state fsm/default]))]

          (let [value' (->> (get-in fsm [:state->input->actions state input])
                         (map reducers)
                         (remove nil?)
                         (reduce #(%2 %1) value))]
            (recur
              (signal (.nextInput stream ::eof))
              value'
              (long state')
              start-index
              (inc stream-index)))

          (if restart?
            ::reject
            (recur
              (if (= state 0)
                (signal (.nextInput stream ::eof))
                input)
              value
              0
              stream-index
              (if (= state 0)
                (inc stream-index)
                stream-index))))))))

(defn compile
  [fsm
   {:keys [reducers signal]
    :or {signal identity}}]
  (let [base-fsm fsm
        fsm (core/precompile fsm)
        initially-accepted? (contains? (:accept fsm) 0)]

    (with-meta

      (reify ICompiledAutomaton
        (start [_ initial-value]
          (CompiledAutomatonState.
            initially-accepted?
            nil
            0
            0
            0
            initial-value))
        (find [_ state stream]
          (if (.accepted? ^CompiledAutomatonState state)
            state
            (advance fsm state stream signal reducers true)))
        (advance-stream [_ state stream reject-value]
          (let [state' (advance fsm state stream signal false)]
            (if (identical? ::reject state')
              reject-value
              state'))))
      {:fsm base-fsm})))
