(ns automat.viz
  (:require
    [automat.core :as a]
    [rhizome.dot :as r]
    [rhizome.viz :as v]))

(defn fsm->dot
  [fsm]
  (let [accept? (a/accept fsm)
        adjacent-fn (if (a/deterministic? fsm)
                      identity
                      #(distinct (apply concat %)))
        src+dst->inputs (fn [src dst]
                          (->> (a/transitions fsm src)
                            (filter (if (a/deterministic? fsm)
                                      #(= dst (val %))
                                      #(contains? (val %) dst)))
                            (map key)))]
    (r/graph->dot
      (conj (a/states fsm) nil)
      #(if-not %
         [(a/start fsm)]
         (->> % (a/transitions fsm) vals adjacent-fn))
      :vertical? false
      :node->descriptor (fn [n]
                          (if-not n
                            {:label "", :width 0, :shape :plaintext}
                            {:label (pr-str n)
                             :shape :circle
                             :peripheries (when (accept? n) 2)}))
      :edge->descriptor (fn [src dst]
                          {:label (->> (src+dst->inputs src dst)
                                    (map #(if (= a/epsilon %) "\u03B5" %))
                                    (interpose ",")
                                    (apply str))}))))

(defn view-fsm
  [fsm]
  (-> fsm fsm->dot v/dot->image v/view-image))
