(ns automat.core-simple-check
  (:require
    [clojure.test :refer :all]
    [clojure.set :as set]
    [automat.core :as a]
    [automat.fsm :as fsm]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [clojure.test.check.clojure-test :as ct :refer (defspec)]))

(defn gen-actions [size]
  (gen/one-of
    (concat
      [(gen/return [:concat :a])
       (gen/return [:concat :b])
       (gen/return [:not [[:concat :a]]])]
      (when (pos? size)
        (let [gen-actions' (gen/resize (quot size 4) (gen/sized gen-actions))]
          [(gen/return [:kleene])
           (gen/tuple (gen/return :or) (gen/list gen-actions'))
           (gen/tuple (gen/return :and) (gen/list gen-actions'))
           (gen/tuple (gen/return :difference) (gen/list gen-actions'))])))))

(defn construct-automaton [actions]
  (reduce
    (fn [fsm [action arg]]
      (case action
        :concat [fsm arg]
        :kleene (a/* fsm)
        :or (a/or fsm (construct-automaton arg))
        :and (a/and fsm (construct-automaton arg))
        :difference (a/difference fsm (construct-automaton arg))
        :not (a/not (construct-automaton arg))))
    []
    actions))

(defn accepts? [fsm inputs]
  (let [fsm (a/compile fsm)
        state (a/greedy-find
                fsm
                nil
                inputs)]
    (and (:accepted? state)
      (= 0 (:start-index state))
      (= (count inputs) (:stream-index state)))))

(defspec ^:stress check-closed-over-operations 1e4
  (prop/for-all
    [actions-a (gen/tuple (gen/sized gen-actions))
     actions-b (gen/tuple (gen/sized gen-actions))
     inputs (gen/list (gen/list (gen/elements [:a :b])))]
    (fsm/reset-generations)
    (pr '.) (flush)
    (let [fsm-a (a/parse-automata (construct-automaton actions-a))
          fsm-b (a/parse-automata (construct-automaton actions-b))
          fsm-a' (a/compile fsm-a)
          fsm-b' (a/compile fsm-b)
          a-inputs (set (filter #(accepts? fsm-a' %) inputs))
          b-inputs (set (filter #(accepts? fsm-b' %) inputs))]

      (let [expected (set/union a-inputs b-inputs)
            actual   (let [fsm (a/compile (a/or fsm-a fsm-b))]
                       (filter #(accepts? fsm %) inputs))]
        (assert (= expected (set actual))
          (str "union: " (pr-str expected) " " (pr-str actual))))

      (let [expected (set/intersection a-inputs b-inputs)
            actual   (let [fsm (a/compile (a/and fsm-a fsm-b))]
                       (filter #(accepts? fsm %) inputs))]
        (assert (= expected (set actual))
          (str "intersection: " (pr-str expected) " " (pr-str actual))))

      (let [expected (set/difference a-inputs b-inputs)
            actual   (let [fsm (a/compile (a/difference fsm-a fsm-b))]
                       (filter #(accepts? fsm %) inputs))]
        (assert (= expected (set actual))
          (str "difference: " (pr-str expected) " " (pr-str actual))))

      true)))
