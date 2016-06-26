(ns automat.fsm-test
  (:require
    [automat.fsm :as a]
    #?(:clj [clojure.test :refer :all]
       :cljs [cemerick.cljs.test]))
  #?(:cljs (:require-macros
             [cemerick.cljs.test :refer [deftest is are]])))

(defn accepts-seq? [fsm sequence]
  (let [fsm (a/->dfa fsm)]
    (loop [state (a/start fsm), inputs sequence]
      (if (empty? inputs)
        (contains? (a/accept fsm) state)
        (let [i (first inputs)]
          (if-let [state' (-> fsm (a/input->state state) (get i))]
            (recur state' (rest inputs))
            false))))))

(deftest test-fsms
  (let [f (a/concat
            (a/kleene (a/automaton :a))
            (a/automaton :b)
            (a/kleene (a/automaton :b))
            (a/automaton :a))]
    (is (every?
          #(accepts-seq? f %)
          [[:b :a]
           [:a :b :a]
           [:a :a :b :b :a]
           [:b :b :b :a]]))
    (is (every?
          #(not (accepts-seq? f %))
          [[:a]
           [:b]
           [:b :b]
           [:b :b :b]]))))
