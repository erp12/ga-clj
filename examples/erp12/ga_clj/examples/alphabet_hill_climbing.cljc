(ns erp12.ga-clj.examples.alphabet-hill-climbing
  (:gen-class)
  (:require [erp12.ga-clj.search.hill-climbing :as hc]
            [erp12.ga-clj.toolbox :as tb]))

(def target
  (vec "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(def cur-ind (atom '()))

(defn -main
  "Hill climbs vector of letters in alphabetical order."
  [& _]
  (println
    (hc/run {;; Generates random genomes as a permutation of the target genome.
             :genome-factory     #(shuffle target)
             ;; Individuals are a map containing a scalar `:error` for the genome.
             ;; In this case, we use the hamming distance.
             ;; The `:genome` is added implicitly.
             :individual-factory (fn [gn _] {:error (tb/hamming-distance gn target)})
             ;; Mutation function to produce neighbor
             :mutate              (fn [individual]
                                   (->> individual
                                        :genome
                                        tb/swap-2-genes))
             ;; We compare individuals on the basis of the error values. Lower is better.
             :individual-cmp     (comparator #(< (:error %1) (:error %2)))
             ;; We stop evolution when either:
             ;;   1. We find an individual with zero error or
             ;;   2. We reach 300 generations.
             :stop-fn            (fn [{:keys [step individual max-steps]}]
                                   (when (not= @cur-ind individual)
                                     (reset! cur-ind individual)
                                     (println "Step:" step "\tBest:" individual))
                                   (cond
                                     (= (:error individual) 0) :solution-found
                                     (>= step max-steps) :max-step-reached))
             ;; Steps before random restart
             :steps-before-restart 1000
             ;; Maximum number of hill climbing steps
             :max-steps          300000})))


(comment
  
  (-main)
  
  )