(ns automat.core
  (:require [clojure.set :as set]))

;;;

(defprotocol Automata
  (deterministic? [_] "Returns true if the automata is a DFA, false otherwise.")
  (states [_] "The set of possible states within the automata.")
  (alphabet [_] "The set of possible inputs for the automata.")
  (start [_] "The start state for the automata.")
  (accept [_] "The set of accept states for the automata.")
  (transitions [_ state] "A map of inputs onto a state (if deterministic) or a set of states (if non-deterministic)."))

(def ^:const epsilon "An input representing no input." ::epsilon)

(defn nfa
  "Creates an NFA."
  [start accept state->input->states]
  (let [accept (set accept)
        states (set/union #{start} accept (set (keys state->input->states)))
        alphabet (apply set/union (->> state->input->states vals (map keys) (map set)))]

    (assert
      (every? set? (->> state->input->states vals (mapcat vals)))
      "All target states within an NFA must be a set.")

    (reify Automata
      (deterministic? [_] false)
      (start [_] start)
      (accept [_] accept)
      (states [_] states)
      (alphabet [_] alphabet)
      (transitions [_ state] (get state->input->states state)))))

(defn dfa
  "Creates a DFA."
  [start accept state->input->state]
  (let [accept (set accept)
        states (set/union #{start} accept (set (keys state->input->state)))
        alphabet (apply set/union (->> state->input->state vals (map keys) (map set)))]

    (reify Automata
      (deterministic? [_] true)
      (start [_] start)
      (accept [_] accept)
      (states [_] states)
      (alphabet [_] alphabet)
      (transitions [_ state] (get state->input->state state)))))

;;;

(defn- next-states
  "Gives all possible next states for given pair of state and input, following all epsilon
   transitions."
  [nfa state input]
  (assert (not (deterministic? nfa)))
  (loop [traversed #{}
         pending (-> nfa (transitions state) (get input))]
    (if (empty? pending)
      traversed
      (let [state     (first pending)
            traversed (conj traversed state)
            pending   (set/union
                        (disj pending state)
                        (set/difference
                          (-> nfa (transitions state) (get epsilon))
                          traversed))]
        (recur traversed pending)))))

(defn nfa->dfa [nfa]
  (assert (not (deterministic? nfa)))
  (loop [explore #{ #{(start nfa)} }
         state->input->state {}]
    (if (empty? explore)

      ;; we're done, wrap it up
      (let [set->vec #(if (= 1 (count %)) (first %) (vec %))
            accept (->> state->input->state
                     vals
                     (mapcat vals)
                     (remove #(empty? (set/intersection (accept nfa) %)))
                     set)]
        (dfa
          (start nfa)
          ;; to make it obvious this isn't an NFA, unwrap sets of one and
          ;; turn everything else into a vector
          (map set->vec accept)
          (zipmap
            (map set->vec (keys state->input->state))
            (map
              (fn [input->state]
                (zipmap
                  (keys input->state)
                  (map set->vec (vals input->state))))
              (vals state->input->state)))))
      
      (let [states (first explore)

            ;; all valid inputs for the compound state
            inputs
            (-> (mapcat #(keys (transitions nfa %)) states)
              set
              (disj epsilon))

            ;; a map of inputs onto the next compound state
            input->state
            (zipmap
              inputs
              (map
                (fn [input]
                  (->> states
                    (map #(next-states nfa % input))
                    (apply set/union)))
                inputs))

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
          state->input->state)))))

;;;

;; uses Hopcroft's algorithm: http://en.wikipedia.org/wiki/DFA_minimization#Hopcroft.27s_algorithm
(defn- reduce-states [dfa]
  (assert (deterministic? dfa))
  (let [accept (accept dfa)
        smallest #(if (< (count %1) (count %2)) %1 %2)]
    (loop [remaining-partitions #{accept}
           partitions #{accept (set/difference (states dfa) accept)}
           remaining-states nil]
      (if (empty? remaining-states)

        ;; pop one off, recur with list of candidate states
        (if (empty? remaining-partitions)
          (->> partitions
            (map #(zipmap % (repeat (first %))))
            (apply merge))
          (let [s (first remaining-partitions)]
            (recur
              (disj remaining-partitions s)
              partitions
              (map
                (fn [input]
                  (set
                    (filter
                      #(contains? s (-> dfa (transitions %) (get input)))
                      (states dfa))))
                (alphabet dfa)))))

        ;; repartition
        (let [a (first remaining-states)
              remaining-partitions (atom remaining-partitions)
              partitions (->> partitions
                           (mapcat
                             (fn [b]
                               (let [i (set/intersection a b)]
                                 (if (empty? i)
                                   [b]
                                   (let [d (set/difference b a)]
                                     (if (contains? @remaining-partitions b)
                                       (swap! remaining-partitions
                                         #(-> %
                                            (disj b)
                                            (conj i d)))
                                       (swap! remaining-partitions conj (smallest i d)))
                                     [i d])))))
                           set)]
          (recur
            @remaining-partitions
            partitions
            (rest remaining-states)))))))

(defn minimize-dfa [fsm]
  (assert (deterministic? fsm))
  (let [state->new-state (reduce-states fsm)]
    (dfa
      (start fsm)
      (->> (accept fsm) (map state->new-state) distinct)
      (zipmap
        (states fsm)
        (map #(transitions fsm %) (states fsm))))))

;;;








