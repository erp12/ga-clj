(ns erp12.ga-clj.search.random
  (:require [erp12.ga-clj.individual :as i]))

(defn run
  [{:keys [genome-factory individual-factory stop-fn]}]
  (let [rand-individual #(i/make-individual individual-factory (genome-factory))]
    (loop [step 0
           individual (rand-individual)]
      (let [result (stop-fn {:step step :individual individual})]
        (if (some? result)
          {:step       step
           :result     result
           :individual individual}
          (recur (inc step)
                 (rand-individual)))))))