(ns automat.core-simple-check
  (:require
    [clojure.test :refer :all]
    [clojure.set :as set]
    [automat.core :as a]
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

(def input
  (gen/elements [:a :b]))

(def input-stream
  (gen/list input))

(def input-streams
  (gen/list input-stream))

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

(def closed-over-operations
  (prop/for-all
    [streams input-streams]
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

(defspec check-closed-over-operations 1000
  closed-over-operations)
