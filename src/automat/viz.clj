(ns automat.viz
  (:require
    [automat.core :as c]
    [automat.fsm :as a]
    [rhizome.dot :as r]
    [rhizome.viz :as v])
  (:import
    [automat.core
     ICompiledAutomaton]))

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
  [fsm options]
  (let [fsm (if (instance? ICompiledAutomaton fsm)
              (-> fsm meta :fsm)
              (-> fsm c/parse-automata a/final-minimize))
        state->index (if (instance? ICompiledAutomaton fsm)
                       (-> fsm meta :state->index)
                       (constantly nil))
        accept? (a/accept fsm)
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
      :options options
      :vertical? false
      :node->descriptor (fn [n]
                          (if-not n
                            {:width 0, :shape :plaintext}
                            {:shape :circle
                             :peripheries (when (accept? n) 2)
                             :label (cond
                                      (= a/reject n) "REJ"
                                      (number? (state->index n)) (str (state->index n)))}))
      :edge->descriptor (fn [src dst]
                          {:fontname "monospace"
                           :label (->> (src+dst->inputs src dst)
                                    (map #(cond
                                            (= a/epsilon %) "\u03B5"
                                            (= a/default %) "DEF"
                                            (string? %) (str \" % \")
                                            :else %))
                                    pprint-inputs)}))))

(defn view
  "Displays the states and transitions of `fsm`."
  ([fsm]
     (view fsm nil))
  ([fsm options]
     (-> fsm 
       (fsm->dot options)
       v/dot->image
       v/view-image)))

(defn save
  "Renders the states and transitions of `fsm`, and saves them to `filename`."
  ([fsm filename]
     (save fsm filename nil))
  ([fsm filename options]
     (-> fsm 
       (fsm->dot options)
       v/dot->image
       (v/save-image filename))))
