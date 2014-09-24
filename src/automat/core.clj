(ns automat.core
  (:refer-clojure :exclude
    [concat compile find .. * + or and not complement])
  (:use
    [potemkin])
  (:require
    [clojure.set :as set]
    [proteus :refer (let-mutable)]
    [automat.compiler.core :as core]
    [automat.compiler.eval :as eval]
    [automat.fsm :as fsm]
    [automat.stream :as stream]
    [primitive-math :as p])
  (:import
    [automat.utils
     InputStream]
    [automat.fsm
     IAutomaton]
    [automat.compiler.core
     ICompiledAutomaton
     CompiledAutomatonState]))

;;;

(import-vars
  [automat.compiler.core
   find
   advance-stream
   parse-automata
   parse-input
   ->automaton-state
   normalize-input])

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

(defn greedy-find
  "Greedily find the largest possible accepted sub-sequence.  Will only return an `accepted?` state once subsequent
   inputs have been rejected.  Since this always consumes more inputs than the accepted sub-sequence, be careful when
   using impure functions or input sources."
  [fsm state stream]
  (let [^CompiledAutomatonState state (->automaton-state fsm state)
        stream (stream/to-stream stream)]
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
     (let [state (->automaton-state fsm state)]
       (advance-stream fsm state [(normalize-input input)] reject-value))))

;;; define these last, so we don't use them mistakenly above

(defn or
  "Returns an automaton that accepts the union of the given automata."
  [& args]
  (->> args
    (map parse-automata)
    (apply fsm/union)
    fsm/minimize))

(defn and
  "Returns an automaton that accepts the intersection the given automata."
  [& args]
  (->> args
    (map parse-automata)
    (apply fsm/intersection)
    fsm/minimize))

(defn difference
  "Returns an automaton that accepts the disjoint of the first automaton and the subsequent
   automata."
  [& args]
  (->> args
    (map parse-automata)
    (apply fsm/difference)
    fsm/minimize))

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

(def ^{:doc "Returns an automaton that immediately accepts."}
  none (fsm/empty-automaton))

;;;



(defn compile
  "Compiles the fsm into something that can be used with `find`, `advance-stream`, `greedy-find`, and `advance`.  Optionally takes an option map, which may contain:

    `reducers` - a map or function of actions defined via `$` onto reducer functions which take two arguments, the current reduction value and the input.

    `signal` - a function that takes an input and returns the signal that will be used to advance the automaton.  The input passed into reducer functions will not be affected by this."
  ([fsm]
     (compile fsm nil))
  ([fsm {:keys [reducers signal] :as options}]
     (if (instance? ICompiledAutomaton fsm)
       fsm
       (let [fsm (fsm/final-minimize (parse-automata fsm))]
         (eval/compile fsm options)))))
