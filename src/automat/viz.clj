(ns automat.viz
  (:require
    [automat.fsm :as a]
    [rhizome.dot :as r]
    [rhizome.viz :as v]))

(defn- pprint-inputs
  "Turns contiguous ranges from a to b into 'a..b'"
  [s]
  (let [non-numbers (remove number? s)
        s (a/input-ranges (filter number? s))]
    (->> s
      (map
        #(cond
           (and (vector? %) (not= (first %) (second %)))
           (str (first %) ".." (second %))

           (vector? %)
           (first %)

           :else
           %))
      (concat non-numbers)
      (interpose ",")
      (apply str))))

(defn fsm->dot
  ([fsm]
     (fsm->dot fsm (constantly nil)))
  ([fsm state->index]
     (let [accept? (a/accept fsm)
           adjacent-fn (if (a/deterministic? fsm)
                         distinct
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
                               {:width 0, :shape :plaintext}
                               {:shape :circle
                                :peripheries (when (accept? n) 2)
                                :label (when (number? (state->index n))
                                         (str (state->index n)))}))
         :edge->descriptor (fn [src dst]
                             {:fontname "monospace"
                              :label (->> (src+dst->inputs src dst)
                                       (map #(cond
                                               (= a/epsilon %) "\u03B5"
                                               (= a/default %) "DEF"
                                               :else %))
                                       pprint-inputs)})))))

(defn view-fsm
  ([fsm]
     (view-fsm fsm (constantly nil)))
  ([fsm state->index]
     (-> fsm (fsm->dot state->index) v/dot->image v/view-image)))
