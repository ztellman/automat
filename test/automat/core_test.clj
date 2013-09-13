(ns automat.core-test
  (:require
    [clojure.test :refer :all]
    [automat.core :as a]
    [criterium.core :as c])
  (:import
    [java.nio
     ByteBuffer]))

(defn accepts? [f fsm start-index stream-index s]
  (let [state (f fsm (a/start fsm nil) s)]
    (and
      (:accepted? state)
      (= start-index (:start-index state))
      (= stream-index (:stream-index state)))))

(deftest test-find
  (are [fsm input-seqs]
    (let [fsm' (a/compile fsm)]
      (every?
        (fn [[start end s]]
          (accepts? a/find fsm' start end s))
        (partition 3 input-seqs)))

    a/any
    [0 1 [1]
     0 1 [2 3]
     0 1 [3 3 3]]

    (a/+ :a)
    [0 1 [:a :a]
     1 2 [:b :a :a]]

    (a/+ 1)
    [0 1 [1 1]
     1 2 [0 1 1]]

    (a/fsm 1 2 3)
    [0 3 [1 2 3]
     2 5 [1 1 1 2 3 2 1]]

    (a/or
      (a/fsm 1 2 3)
      (a/fsm 2 3))
    [0 3 [1 2 3]
     0 2 [2 3]
     1 3 [0 2 3]]

    ))

(deftest test-greedy-find
  (are [fsm input-seqs]
    (let [fsm' (a/compile fsm)]
      (every?
        (fn [[start end s]]
          (accepts? a/greedy-find fsm' start end s))
        (partition 3 input-seqs)))

    a/any
    [0 1 [1]
     0 1 [2 3]]

    (a/+ 1)
    [0 2 [1 1]
     1 3 [0 1 1 0]]

    ))

(deftest ^:benchmark benchmark-find
  (let [buf (->> (cycle [1 2 3])
              (take 1e6)
              (map byte)
              byte-array
              ByteBuffer/wrap)
        fsm (a/compile (a/fsm 1 2 3 4))]
    (println "find within a 1mb buffer")
    (c/bench
      (a/find fsm (a/start fsm nil) buf))))
