(ns erp12.ga-clj.search.brute-force
  "Brute force generation of genomes, from shortest to longest."
  (:require [erp12.ga-clj.individual :as i]
            [clojure.math.combinatorics :as combo]))

(defn brute-force-lazy-sequence
  "Given a genetic source, produce a lazy sequence consisting of:
   - all 0 element sequences of genes from the genetic source
   - all 1 element sequences of genes from the genetic source
   - all 2 element sequences of genes from the genetic source
   - all 3 element sequences of genes from the genetic source
   - etc."
  [genetic-source]
  (apply concat
         (map (partial combo/selections genetic-source)
              (range))))

(defn run
  "Brute force generation of genomes. Requires a sequence of genes (genetic-source)
   that will be used as elements of genomes."
  [{:keys [genetic-source individual-factory stop-fn max-steps]}]
  (loop [step 0
         individual-seq (brute-force-lazy-sequence genetic-source)]
    (let [individual (i/make-individual individual-factory (first individual-seq))
          result (stop-fn {:step step :individual individual :max-steps max-steps})]
      (if (some? result)
        {:step       step
         :result     result
         :individual individual}
        (recur (inc step)
               (rest individual-seq))))))
