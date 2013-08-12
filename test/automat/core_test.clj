(ns automat.core-test
  (:require
    [clojure.test :refer :all]
    [automat.core :as a]))

(defn accept? [dfa sequence]
  (assert (a/deterministic? dfa))
  (loop [state (a/start dfa), inputs sequence]
    (if (empty? inputs)
      (contains? (a/accept dfa) state)
      (let [i (first inputs)]
        (if-let [state' (-> dfa (a/transitions state) (get i))]
          (recur state' (rest inputs))
          false)))))

(def fsm
  (a/nfa
    1
    #{3}
    {1 {:a #{2}
        :c #{4}}
     2 {:b #{3}
        a/epsilon #{1}}
     3 {:a #{2}}
     4 {:c #{3}
        a/epsilon #{3}}}))


