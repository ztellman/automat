(ns automat.core
  (:refer-clojure :exclude
    [concat compile find])
  (:use
    [potemkin.types])
  (:require
    [proteus :refer (let-mutable)]
    [automat.fsm :as fsm]
    [primitive-math :as p]))

;;;

(definterface+ ICompiledAutomaton
  (start [_])
  (find [_ state input])
  (advance [_ state input]))

(definterface+ ICompiledAutomatonState
  (accepted? [_])
  (state-index ^long [_])
  (stream-index ^long [_]))

(deftype+ CompiledAutomatonState
  [^boolean accepted?
   ^long state-index
   ^long stream-index
   state]
  ICompiledAutomatonState
  (accepted? [_] accepted?)
  (state-index [_] state-index)
  (stream-index [_] stream-index)
  clojure.lang.IDeref
  (deref [_] state))

(defn- jump-table
  [compiled-state input fsm input->index state->index]
  `(let-mutable [accept? false
                 state @compiled-state
                 state-index' -1]
     (case (state-index ~compiled-state)
       ~@(->> state->index
           (mapcat
             (fn [[state index]]
               `(~index
                  (case ~input
                    ~@(->> (fsm/transitions fsm state)
                        keys
                        (mapcat
                          (fn [input]
                            (let [state' (-> fsm
                                           (fsm/transitions state)
                                           (get input))]
                              `(~(input->index index)
                                (do
                                  (set! state-index' ~(state->index state'))
                                  ~@(when ((fsm/accept fsm) state')
                                      `((set! accept? true)))))))))))))))))

(defn compile [fsm]
  (let [fsm (fsm/minimize fsm)
        input->index (zipmap
                       (->> fsm
                         fsm/states
                         (map #(fsm/transitions fsm %))
                         (map keys)
                         distinct)
                       (range))
        state->index (zipmap
                       (fsm/states fsm)
                       (range))]
    (jump-table 'state 'input fsm identity state->index)))
