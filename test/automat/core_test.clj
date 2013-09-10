(ns automat.core-test
  (:require
    [clojure.test :refer :all]
    [automat.core :as a]
    [criterium.core :as c])
  (:import
    [java.nio
     ByteBuffer]))

(defn accepts? [f fsm stream-index s]
  (let [state (f fsm (a/start fsm) s)]
    (and
      (:accepted? state)
      (= stream-index (:stream-index state)))))

(deftest test-find
  (are [fsm input-seqs]
    (let [fsm' (a/compile fsm)]
      (every?
        (fn [[idx s]]
          (accepts? a/find fsm' idx s))
        (partition 2 input-seqs)))

    a/any
    [1 [1]
     1 [2 3]
     1 [3 3 3]]

    (a/+ :a)
    [1 [:a :a]
     2 [:b :a :a]]

    (a/+ 1)
    [1 [1 1]
     2 [0 1 1]]

    (a/fsm 1 2 3)
    [3 [1 2 3]
     5 [1 1 1 2 3 2 1]]

    (a/or
      (a/fsm 1 2 3)
      (a/fsm 2 3))
    [3 [1 2 3]
     2 [2 3]
     3 [0 2 3]]

    ))

(deftest test-greedy-find
  (are [fsm input-seqs]
    (let [fsm' (a/compile fsm)]
      (every?
        (fn [[idx s]]
          (accepts? a/greedy-find fsm' idx s))
        (partition 2 input-seqs)))

    a/any
    [1 [1]
     1 [2 3]]

    (a/+ 1)
    [2 [1 1]
     3 [0 1 1 0]]

    ))

(deftest ^:benchmark benchmark-find
  (let [buf (->> (cycle [1 2 3])
              (take 1e6)
              (map byte)
              byte-array
              ByteBuffer/wrap)
        fsm (a/compile (a/fsm 1 2 3 4))]
    (c/quick-bench
      (a/find fsm (a/start fsm) buf))))
