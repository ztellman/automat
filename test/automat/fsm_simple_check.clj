(ns automat.fsm-simple-check
  (:require
    [clojure.test :refer :all]
    [clojure.set :as set]
    [automat.fsm :as a]
    [simple-check.core :as sc]
    [simple-check.generators :as gen]
    [simple-check.properties :as prop]
    [simple-check.clojure-test :as ct :refer (defspec)]))

;;;

(defn accepts-seq? [fsm sequence]
  (let [fsm (a/->dfa fsm)]
    (loop [state (a/start fsm), inputs sequence]
      (if (empty? inputs)
        (contains? (a/accept fsm) state)
        (let [i (first inputs)]
          (if-let [state' (-> fsm (a/transitions state) (get i))]
            (recur state' (rest inputs))
            false))))))

;;;

(def automatons
  (map
    a/minimize
    (list*
      (a/kleene (a/automaton :a :b))
      (mapcat
        #(list
           (->> % (map a/automaton) (apply a/concat))
           (a/concat
             (->> % (map a/automaton) (apply a/concat))
             (a/kleene (a/automaton :a :b))))
        [[:a] [:b] [:a :b] [:b :a]]))))

(def streams-gen (gen/list (gen/list (gen/elements [:a :b]))))

(defspec check-closed-over-operations 100
  (prop/for-all
    [streams streams-gen]
    (let [accepts #(->> streams (filter (partial accepts-seq? %)) set)]
      (every? true?
        (for [a automatons, b automatons]
          (let [s-a (accepts a)
                s-b (accepts b)]
            (every?
              (fn [[f-f s-f]]
                (= (s-f s-a s-b)
                  (accepts (f-f a b))))
              {a/union set/union
               a/intersection set/intersection
               a/difference set/difference})))))))
