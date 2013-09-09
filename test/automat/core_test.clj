(ns automat.fsm-test
  (:require
    [clojure.test :refer :all]
    [automat.core :as a]
    [automat.stream :as s]))

(defn accepts? [fsm s]
  (:accepted? (a/find fsm (a/start fsm) (s/to-stream s))))

(deftest test-fsm-accepts
  (are [fsm input-seqs]
    (let [fsm' (a/compile fsm)]
      (every? (partial accepts? fsm') input-seqs))

    a/any
    [[1]
     [2 3]
     [3 3 3]]

    (a/fsm 1 2 3)
    [[1 2 3]
     [1 1 1 2 3 2 1]]

    ))
