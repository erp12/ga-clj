(ns erp12.ga-clj.search.hill-climbing
  "Random-restart hill climbing using first better neighbor at each step.
   If spends steps-before-restart steps without finding better neighbor,
   randomly restarts."
  (:require [erp12.ga-clj.individual :as i]
            [erp12.ga-clj.utils :as u]))

(defn run
  "Random-restart hill climbing using first better neighbor at each step.
   If spends steps-before-restart steps without finding better neighbor,
   randomly restarts."
  [{:keys [genome-factory individual-factory stop-fn individual-cmp mutate steps-before-restart]}]
  (let [rand-individual #(i/make-individual individual-factory (genome-factory))]
    (loop [step 0
           steps-on-current-individual 0
           restarts 0
           individual (rand-individual)]
      (let [result (stop-fn {:step step :individual individual})]
        (cond (some? result)
              {:step       step
               :result     result
               :individual individual
               :restarts   restarts}

              (>= steps-on-current-individual steps-before-restart)
              (recur (inc step)
                     0
                     (inc restarts)
                     (rand-individual))

              ;; Create neighbor through mutation and keep better (lower error)
              ;; of individual and neighbor. If same, keep individual
              :else
              (let [neighbor (i/make-individual individual-factory (mutate individual))]
                (if (< 0 (individual-cmp individual neighbor))
                  (recur (inc step)
                         0
                         restarts
                         neighbor)
                  (recur (inc step)
                         (inc steps-on-current-individual)
                         restarts
                         individual))))))))
