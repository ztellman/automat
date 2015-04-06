(ns automat.viz
  (:require
    [automat.compiler.core :as compiler]
    [clojure.set :as set]
    [automat.core :as c]
    [automat.fsm :as a]
    [rhizome.dot :as r])
  (:import
    [automat.compiler.core
     ICompiledAutomaton]))

(defn- pprint-inputs
  "Turns contiguous ranges from a to b into 'a..b'"
  [s]
  (let [number? #(or (number? %) (char? %))
        non-numbers (remove number? s)
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
  (let [fsm (compiler/precompile
              (if (instance? ICompiledAutomaton fsm)
                (-> fsm meta :fsm)
                fsm))
        states (compiler/states fsm)
        accept? (:accept fsm)
        src+dst->inputs (fn [src dst]
                          (->> (get-in fsm [:state->input->state src])
                            (filter #(= dst (val %)))
                            (map key)))]
    (r/graph->dot
      (conj states nil)
      #(if-not %
         [0]
         (->> (get-in fsm [:state->input->state %]) vals distinct))
      :options options
      :vertical? false
      :node->descriptor (fn [n]
                          (if-not n
                            {:width 0, :shape :plaintext}
                            {:shape :circle
                             :peripheries (when (accept? n) 2)
                             :label (cond
                                      (= a/reject n) "REJ"
                                      :else n)}))
      :edge->descriptor (fn [src dst]
                          (let [pre-actions (get-in fsm [:state->input->actions src a/pre])]
                            (if (nil? src)

                              ;; entry to start state
                              (when-not (empty? pre-actions)
                                {:fontname "monospace"
                                 :label (apply str "/ " (interpose ", " pre-actions))})

                              ;; all others
                              (->> (src+dst->inputs src dst)
                                (group-by
                                  #(set/union
                                     (get-in fsm [:state->input->actions src %])
                                     pre-actions))
                                (map
                                  (fn [[actions inputs]]
                                    (let [inputs' (map
                                                    #(cond
                                                       (= a/epsilon %) "\u03B5"
                                                       (= a/default %) "DEF"
                                                       (string? %) (str \" % \")
                                                       :else %)
                                                    inputs)]
                                      (apply str
                                        (pprint-inputs inputs')
                                        (when-not (empty? actions)
                                          (list* " / "
                                            (interpose ", " actions)))))))
                                (map #(hash-map :fontname "monospace" :label %))
                                vec)))))))

(defn view
  "Displays the states and transitions of `fsm`."
  ([fsm]
     (view fsm {:dpi 100}))
  ([fsm options]
     (require 'rhizome.viz)
     (let [dot->image (resolve 'rhizome.viz/dot->image)
           view-image (resolve 'rhizome.viz/view-image)]
       (-> fsm
         (fsm->dot options)
         dot->image
         view-image))))

(defn save
  "Renders the states and transitions of `fsm`, and saves them to `filename`."
  ([fsm filename]
     (save fsm filename nil))
  ([fsm filename options]
     (require 'rhizome.viz)
     (let [dot->image (resolve 'rhizome.viz/dot->image)
           save-image (resolve 'rhizome.viz/save-image)]
       (-> fsm
         (fsm->dot options)
         dot->image
         (save-image filename)))))
