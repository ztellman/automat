(ns automat.fsm
  (:refer-clojure :exclude
    [concat complement])
  (:use
    [potemkin.types])
  (:require
    [primitive-math :as p]
    [clojure.set :as set])
  (:import
    [java.util
     LinkedList]))

;;;

(let [cnt (atom 0)]
  (defn- new-generation []
    (swap! cnt inc))
  (defn reset-generations []
    (reset! cnt 0)))

(deftype+ State
  [generation
   descriptor
   sub-states
   action]
  Object
  (hashCode [_]
    (p/+
      (hash generation)
      (hash descriptor)
      (hash sub-states)))
  (equals [_ x]
    (and
      (instance? State x)
      (let [^State x x]
        (and
          (= generation (.generation x))
          (= descriptor (.descriptor x))
          (= sub-states (.sub-states x)))))))

(defn- assoc-action [^State s action]
  (State.
    (.generation s)
    (.descriptor s)
    (.sub-states s)
    action))

(def ^:const epsilon "An input representing no input." ::epsilon)
(def ^:const default "An input representing a default" ::default)
(def ^:const pre "An input representing a pre-action" ::pre)
(def ^:const reject  "A state representing rejection"  ::rejection)

(defn- conj-generation [^State s gen]
  (if (identical? reject s)
    reject
    (State.
      (conj (.generation s) gen)
      (.descriptor s)
      (.sub-states s)
      (.action s))))

(defn- state
  ([]
     (state nil))
  ([x]
     (cond
       (instance? State x) x
       (identical? x reject) x
       :else (State. nil x #{} nil))))

(defn action [^State s]
  (when (instance? State s)
    (.action s)))

(defn- join-states
  ([^State s]
     (if (identical? reject s)
       s
       (State. (.generation s) (.descriptor s) (.sub-states s) nil)))
  ([s & rest]
     (State. nil nil (vec (list* s rest)) nil)))

;;;

(definterface+ IAutomaton
  (deterministic? [_] "Returns true if the automata is a DFA, false otherwise.")
  (states [_] "The set of possible states within the automata.")
  (alphabet [_] "The set of possible inputs for the automata.")
  (start [_] "The start state for the automata.")
  (accept [_] "The set of accept states for the automata.")
  (input->state [_ state] "A map of inputs onto a state (if deterministic) or a set of states (if non-deterministic).")
  (input->actions [_ state] "A map of inputs onto actions for a given state")
  (gensym-states [_]))

(defn actions
  "Return set of actions for the given state and input."
  [fsm state input]
  (get (input->actions fsm state) input))

(defn nfa
  "Creates an NFA."
  [start accept state->input->states state->input->actions]

  (assert
      (every? set? (->> state->input->states vals (mapcat vals)))
      "All target states within an NFA must be a set.")

  (let [map-states (fn [state->input->states f]
                     (zipmap
                       (map f (keys state->input->states))
                       (map
                         (fn [input->states]
                           (zipmap
                             (keys input->states)
                             (map
                               #(set (map f %))
                               (vals input->states))))
                         (vals state->input->states))))
        start (state start)
        accept (set (map state accept))
        state->input->states (map-states state->input->states state)
        state->input->actions (zipmap
                                (map state (keys state->input->actions))
                                (vals state->input->actions))
        states (set/union
                 #{start}
                 accept
                 (->> state->input->states keys set)
                 (->> state->input->states vals (mapcat vals) (apply set/union)))
        alphabet (apply set/union (->> state->input->states vals (map keys) (map set)))]
    (reify IAutomaton
      (deterministic? [_] false)
      (start [_] start)
      (accept [_] accept)
      (states [_] states)
      (alphabet [_] alphabet)
      (input->state [_ state] (get state->input->states state))
      (input->actions [_ state] (get state->input->actions state))
      (gensym-states [_]
        (let [gen (new-generation)
              f #(conj-generation % gen)]
          (nfa
            (f start)
            (map f accept)
            (map-states state->input->states f)
            (zipmap
              (map f (keys state->input->actions))
              (vals state->input->actions))))))))

(defn dfa
  "Creates a DFA."
  [start accept state->input->state state->input->actions]
  (let [map-states (fn [state->input->state f]
                     (zipmap
                       (map f (keys state->input->state))
                       (map
                         (fn [input->state]
                           (zipmap
                             (keys input->state)
                             (map f (vals input->state))))
                         (vals state->input->state))))
        start (state start)
        accept (set (map state accept))
        state->input->state (map-states state->input->state state)
        state->input->actions (zipmap
                                (map state (keys state->input->actions))
                                (vals state->input->actions))
        states (set/union
                 #{start}
                 accept
                 (->> state->input->state keys set)
                 (->> state->input->state vals (mapcat vals) set))
        alphabet (apply set/union (->> state->input->state vals (map keys) (map set)))]

    (reify IAutomaton
      (deterministic? [_] true)
      (start [_] start)
      (accept [_] accept)
      (states [_] states)
      (alphabet [_] alphabet)
      (input->state [_ state] (get state->input->state state))
      (input->actions [_ state] (get state->input->actions state))
      (gensym-states [_]
        (let [gen (new-generation)
              f #(conj-generation % gen)]
          (dfa
            (f start)
            (map f accept)
            (map-states state->input->state f)
            (zipmap
              (map f (keys state->input->actions))
              (vals state->input->actions))))))))

(defn pprint-dfa [fsm]
  (assert (deterministic? fsm))
  (let [state->id (zipmap (states fsm) (range))]
    (println "start: " (state->id (start fsm)))
    (println "accept: " (map state->id (accept fsm)))
    (prn
      (into (sorted-map)
        (zipmap
          (map state->id (states fsm))
          (map
            (fn [state]
              (let [t (input->state fsm state)
                    h (input->actions fsm state)]
                (into (sorted-map)
                  (zipmap
                    (keys t)
                    (map #(vector (get h %) (state->id %)) (vals t))))))
            (states fsm)))))))

;;;

(defn- zipmap* [keys f]
  (zipmap keys (map f keys)))

(defn- intersects? [a b]
  (not (empty? (set/intersection a b))))

(defn- next-states
  "Gives all possible next states for given pair of state and input
   transitions."
  [nfa state input]
  (assert (not (deterministic? nfa)))
  (loop [traversed #{}
         pending (-> nfa (input->state state) (get input))]
    (if (empty? pending)
      traversed
      (let [state     (first pending)
            traversed (conj traversed state)
            pending   (set/union
                        (disj pending state)
                        (set/difference
                          (-> nfa (input->state state) (get epsilon))
                          traversed))]
        (recur traversed pending)))))

(defn next-state
  "Returns the next state given a state and input. Assumes a DFA."
  [dfa state input]
  (assert (deterministic? dfa))
  (get (input->state dfa state) input
    (get (input->state dfa state) default)))

(defn ->nfa
  "Converts the given automaton into a non-deterministic finite automata. If it's already
   non-deterministic, this is a no-op."
  [fsm]
  (if-not (deterministic? fsm)
    fsm
    (nfa
      (start fsm)
      (accept fsm)
      (zipmap
        (states fsm)
        (map
          (fn [input->state]
            (zipmap
              (keys input->state)
              (map #(set [%]) (vals input->state))))
          (map #(input->state fsm %) (states fsm))))
      (zipmap* (states fsm) #(input->actions fsm %)))))

(defn ->dfa
  "Converts the given automaton into a deterministic finite automata. If it's already
   deterministic, this is a no-op."
  [fsm]
  (if (deterministic? fsm)
    fsm
    (let [start-state (conj
                         (next-states fsm (start fsm) epsilon)
                         (start fsm))
          start-actions (->> start-state
                          (map action)
                          (remove nil?)
                          set)]
      (loop [explore #{start-state}
             state->input->state {}]
       (if (empty? explore)

         ;; we're done, wrap it up
         (let [accept (->> state->input->state
                        vals
                        (mapcat vals)
                        (clojure.core/concat (keys state->input->state))
                        (filter #(intersects? (accept fsm) %))
                        set)]
           (dfa
             (apply join-states start-state)
             (map #(apply join-states %) accept)

             ;; state->input->state
             (zipmap
               (map #(apply join-states %) (keys state->input->state))
               (map
                 (fn [input->state]
                   (zipmap
                     (keys input->state)
                     (map #(apply join-states %) (vals input->state))))
                 (vals state->input->state)))

             ;; state->input->actions
             (let [state->input->actions
                   (zipmap
                     (map #(apply join-states %) (keys state->input->state))
                     (map
                       (fn [states]
                         (let [input->states (state->input->state states)
                               m (->> states
                                   (map #(input->actions fsm %))
                                   (apply merge-with set/union))
                               m' (zipmap* (keys input->states)
                                    (fn [input]
                                      (->> input
                                        input->states
                                        (map action)
                                        (remove nil?)
                                        set)))]
                           (merge-with set/union m m')))
                       (keys state->input->state)))]
               (update-in state->input->actions
                 [(apply join-states start-state) pre]
                 set/union
                 start-actions))))

         (let [states (first explore)

               ;; all valid inputs for the compound state
               inputs
               (-> (mapcat #(keys (input->state fsm %)) states)
                 set
                 (disj epsilon))

               ;; a map of inputs onto the next compound state
               input->state
               (zipmap*
                 inputs
                 (fn [input]
                   (->> states
                     (map #(next-states fsm % input))
                     (apply set/union))))

               state->input->state
               (assoc state->input->state
                 states input->state)]
           (recur
             (set/union
               (disj explore states)
               (->> input->state
                 vals
                 (remove #(contains? state->input->state %))
                 set))
             state->input->state)))))))

(defn add-action
  [fsm action]
  (let [fsm (->dfa fsm)]
    (dfa
      (start fsm)
      (accept fsm)
      (zipmap* (states fsm) #(input->state fsm %))
      (zipmap* (states fsm)
        (fn [state]
          (->> (input->actions fsm state)
            (map
              (fn [[input actions]]
                [input
                 (if (identical? pre input)
                   actions
                   (conj (or actions #{}) action))]))
            (into {})))))))

;;;

;; assumes DFA
(defn- reachable-states
  "Returns states which can be reached from the start state."
  [fsm]
  (loop [reachable #{ (start fsm) }
         explore #{ (start fsm) }]
    (if (empty? explore)
      reachable
      (let [explore' (->> explore
                       (map #(->> % (input->state fsm) vals set))
                       (apply set/union))]
        (recur
          (set/union reachable explore')
          (set/difference explore' reachable))))))

;; assumes DFA
(defn dead-states
  "Returns non-accept states which point only to themselves and other dead states."
  [fsm]
  (let [candidates (set/difference (states fsm) (accept fsm))]
    (loop [dead #{}]
      (if-let [dead' (seq
                       (filter
                         (fn [state]
                           (-> (input->state fsm state)
                             vals
                             set
                             (disj state)
                             (set/difference dead)
                             empty?))
                         (set/difference
                           candidates
                           dead)))]
        (recur (set/union (set dead') dead))
        dead))))

;; union-find, basically
(defn- merge-pairs [pairs]
  (let [m (reduce
            (fn [m [a b]]
              (let [x (get m a (get m b a))]
                (assoc m a x b x)))
            {}
            pairs)
        root (fn root [x]
               (let [x' (get m x)]
                 (if (= x x')
                   x
                   (recur x'))))]
    (zipmap* (keys m) root)))

(defn- reduce-states [fsm]
  (let [accept (accept fsm)
        states (states fsm)
        state->index (zipmap states (range))
        other (set/difference states accept)
        tuple #(if (< (state->index %1) (state->index %2)) [%1 %2] [%2 %1])
        cartesian #(distinct (for [x % y %] (tuple x y)))
        s (->>
            (clojure.core/concat
              (cartesian accept)
              (cartesian other))
            (filter
              (fn [[a b]]
                (= (actions fsm a pre)
                  (actions fsm b pre))))
            set)]
    (loop [equivalent s, prev nil]
      (if (= prev equivalent)
        (let [state->state' (merge
                              (zipmap states states)
                              (merge-pairs equivalent))
              state'->states (group-by state->state' (keys state->state'))]
          (zipmap*
            (keys state->state')
            (fn [state]
              (->> state state->state' state'->states (apply join-states)))))
        (recur
          (reduce
            (fn [equivalent [a b]]
              (let [inputs (->
                             (clojure.core/concat
                               (keys (input->state fsm a))
                               (keys (input->state fsm b)))
                             set)]
                (if-not (every?
                            #(let [a' (next-state fsm a %)
                                   b' (next-state fsm b %)]
                               (and
                                 (= (actions fsm a :pre) (actions fsm b :pre))
                                 (= (actions fsm a %) (actions fsm b %))
                                 (if (and a' b')
                                   (equivalent (tuple a' b'))
                                   (= a' b'))))
                            inputs)
                  (disj equivalent [a b])
                  equivalent)))
            equivalent
            equivalent)
          equivalent)))))

(defn- prune [fsm]
  (let [fsm (->dfa fsm)
        reachable? (reachable-states fsm)]
    (dfa
      (start fsm)
      (filter reachable? (accept fsm))
      (zipmap*
        (filter reachable? (states fsm))
        (fn [state]
          (->> (input->state fsm state)
            (filter #(reachable? (val %)))
            (into {}))))
      (zipmap*
        (filter reachable? (states fsm))
        #(input->actions fsm %)))))

(defn minimize
  "Returns a minimized DFA."
  [fsm]
  (let [fsm (prune (->dfa fsm))
        state->new-state (reduce-states fsm)
        new-state->states (group-by state->new-state (keys state->new-state))

        state->input->state
        (zipmap*
          (keys new-state->states)
          (fn [new-state]
            (let [state (-> new-state new-state->states first)]
              (->> new-state
                new-state->states
                (map
                  (fn [state]
                    (let [input->state (input->state fsm state)]
                      (zipmap
                        (keys input->state)
                        (map state->new-state (vals input->state))))))
                (apply merge)))))

        state->input->actions
        (zipmap*
          (keys new-state->states)
          (fn [new-state]
            (let [state (-> new-state new-state->states first)]
              (->> new-state
                new-state->states
                (map #(input->actions fsm %))
                (apply merge-with set/union)))))]
    (dfa
      (state->new-state
        (start fsm))
      (->> (accept fsm)
        (map state->new-state)
        set)
      state->input->state

      ;; remove duplicate transition-based actions if the destination
      ;; state has the same 'pre' action
      (zipmap*
        (keys state->input->actions)
        (fn [state]
          (let [input->actions (get state->input->actions state)]
            (zipmap* (keys input->actions)
              (fn [input]
                (set/difference
                  (get input->actions input)
                  (get-in state->input->actions
                    [(get-in state->input->state [state input])
                     pre]))))))))))

(defn final-minimize
  "Marks dead states as `reject`, or removes them altogether if there's no default input.
   This should only be used in conjunction with `compile`."
  [fsm]
  (let [fsm (minimize fsm)
        dead? (dead-states fsm)]
    (minimize
      (dfa
        (start fsm)
        (accept fsm)
        (zipmap*
          (set/difference (states fsm) dead?)
          (fn [state]
            (let [default-state (next-state fsm state default)
                  input->state (input->state fsm state)
                  default? (contains? input->state default)]
              (->> input->state
                (filter (fn [[k v]]
                          (or default?
                            (not (dead? v)))))
                (map (fn [[k v]] [k (if (dead? v) reject v)]))
                (into {})))))
        (zipmap*
          (set/difference (states fsm) dead?)
          #(input->actions fsm %))))))

(defn input-ranges [s]
  (let [f (fn [a b]
            (cond

              (and (number? a) (number? b))
              (compare a b)

              (and (char? a) (char? b))
              (compare a b)

              :else
              (compare (str (class a)) (str (class b)))))]
    (loop [accumulator [], start nil, end nil, s (sort-by identity f s)]
      (if (empty? s)
        (if end
          (conj accumulator [start end])
          accumulator)
        (let [x (first s)]
          (cond

            (and end (== (inc (int end)) (int x)))
            (recur accumulator start x (rest s))

            end
            (recur (conj accumulator [start end]) x x (rest s))

            :else
            (recur accumulator x x (rest s))))))))

;;;

(defn automaton
  "A basic automaton that will accept any of the given inputs."
  [& inputs]
  (dfa 0 #{1} {0 (zipmap inputs (repeat 1))} nil))

(defn empty-automaton
  "A basic automaton that takes no inputs, and immediately accepts."
  [& inputs]
  (dfa 0 #{0} {} nil))

(defn action-automaton
  [action-name]
  (let [s (->State nil (gensym (name action-name)) #{} action-name)]
    (dfa s #{s} {} nil)))

(defn all-automaton
  "A basic automaton that accepts all inputs."
  []
  (dfa 0 #{1} {0 {default 1}} nil))

(defn concat
  "Concatenate one or more automatons together."
  ([a]
     a)
  ([a b]
     ;; remove epsilons before adding more
     (let [a (-> a ->dfa gensym-states ->nfa)
           b (-> b ->dfa gensym-states ->nfa)
           state->input->states (merge
                                  (zipmap* (states a) #(input->state a %))
                                  (zipmap* (states b) #(input->state b %)))]
       (nfa
         (start a)
         (accept b)
         (reduce
           #(assoc-in %1 [%2 epsilon] #{(start b)})
           state->input->states
           (accept a))
         (zipmap* (keys state->input->states)
           (fn [s]
             (or
               (input->actions a s)
               (input->actions b s)))))))
  ([a b & rest]
     (apply concat (concat a b) rest)))

(defn kleene
  "Accepts zero or more of the given automaton."
  [fsm]
  ;; remove epsilons before adding more
  (let [fsm (-> fsm ->dfa ->nfa)]
    (nfa
      (start fsm)
      (conj (accept fsm) (start fsm))
      (reduce
        #(assoc-in %1 [%2 epsilon] #{(start fsm)})
        (zipmap* (states fsm) #(input->state fsm %))
        (accept fsm))
      (zipmap* (states fsm) #(input->actions fsm %)))))

(defn maybe
  "Accepts one or zero of the given automaton."
  [fsm]
  ((if (deterministic? fsm) dfa nfa)
   (start fsm)
   (conj (accept fsm) (start fsm))
   (zipmap* (states fsm) #(input->state fsm %))
   (zipmap* (states fsm) #(input->actions fsm %))))

(defn- merge-fsms [a b accept-states actions]
  (let [a (gensym-states (->dfa a))
        b (gensym-states (->dfa b))
        cartesian-states (for [s-a (states a), s-b (states b)]
                           (join-states s-a s-b))
        inputs (set/union
                 (alphabet a)
                 (alphabet b))

        start-state (state)

        state->input->state
        (merge
          (zipmap* (states a) #(input->state a %))
          (zipmap* (states b) #(input->state b %))
          (zipmap*
            cartesian-states
            (fn [^State state]
              (let [sub-states (.sub-states state)
                    [s-a s-b] sub-states]
                (merge
                  (input->state a s-a)
                  (input->state b s-b)
                  (zipmap*
                    (filter
                      #(and
                         (next-state a s-a %)
                         (next-state b s-b %))
                      inputs)
                    (fn [input]
                      (join-states
                        (next-state a s-a input)
                        (next-state b s-b input)))))))))

        state->input->actions
        (merge
          (zipmap* (states a) #(input->actions a %))
          (zipmap* (states b) #(input->actions b %))
          (zipmap*
            cartesian-states
            (fn [^State state]
              (let [[s-a s-b] (.sub-states state)]
                (actions a b s-a s-b)
                #_(merge-with set/union
                  (input->actions a s-a)
                  (input->actions b s-b))))))]

    (prune
      (dfa
        (join-states (start a) (start b))
        (accept-states a b)
        state->input->state
        state->input->actions))))

(defn complement
  "Returns the complement of the given automaton."
  [fsm]
  ((if (deterministic? fsm) dfa nfa)
   (start fsm)
   (set/difference (states fsm) (accept fsm))
   (zipmap* (states fsm) #(input->state fsm %))
   (zipmap* (states fsm) #(input->actions fsm %))))

(defn intersection
  "Returns the intersection of multiple automata."
  ([a]
     a)
  ([a b]
     (merge-fsms a b
       (fn [a b]
         (for [s-a (accept a), s-b (accept b)]
           (join-states s-a s-b)))
       (fn [a b s-a s-b]
         (merge-with set/union
           (input->actions a s-a)
           (input->actions b s-b)))))
  ([a b & rest]
     (apply intersection (intersection a b) rest)))

(defn union
  "Returns the union of multiple automata."
  ([a]
     a)
  ([a b]
     (merge-fsms a b
       (fn [a b]
         (set/union
           (accept a)
           (accept b)
           (set
             (for [s-a (accept a), s-b (states b)]
               (join-states s-a s-b)))
           (set
             (for [s-a (states a), s-b (accept b)]
               (join-states s-a s-b)))))
       (fn [a b s-a s-b]
         (let [s-a-default (actions a s-a default)
               s-b-default (actions b s-b default)
               inputs (->> [(input->actions a s-a)
                            (input->actions b s-b)]
                        (map keys)
                        (apply clojure.core/concat)
                        distinct)]
           (zipmap*
             inputs
             (fn [input]
               (apply set/union
                 (actions a s-a input)
                 (actions b s-b input)
                 (when-not (= pre input)
                   [s-a-default s-b-default]))))))))
  ([a b & rest]
     (apply union (union a b) rest)))

(defn difference
  "Returns the difference of multiple automata."
  ([a]
     a)
  ([a b]
     (merge-fsms a b
       (fn [a b]
         (set/union
           (accept a)
           (set
             (for [s-a (accept a), s-b (set/difference (states b) (accept b))]
               (join-states s-a s-b)))))
       (fn [a b s-a s-b]
         (merge-with set/difference
           (input->actions a s-a)
           (input->actions b s-b)))))
  ([a b & rest]
     (apply difference (difference a b) rest)))

;;;

(defn matching-inputs
  "Returns a lazy sequence of input sequences which the automaton will match."
  [fsm]
  (let [fsm (-> fsm ->dfa final-minimize)
        accept? (set (accept fsm))
        q (doto (LinkedList.)
            (.add [(start fsm) []]))]
    (take-while
      #(not (identical? ::none %))
      (repeatedly
        (fn []
          (loop []
            (if-let [[state path] (.poll q)]
              (do
                (doseq [[i s] (input->state fsm state)]
                  (.add q [s (conj path i)]))
                (if (accept? state)
                  path
                  (recur)))
              ::none)))))))
