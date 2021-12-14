(ns erp12.ga-clj.examples.alphabet-lexicase
  (:gen-class)
  (:require [erp12.ga-clj.generational :as ga]
            [erp12.ga-clj.toolbox :as tb]))

(def target
  (vec "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(def lexicase
  (tb/make-lexicase-selection {:epsilon nil}))

(defn -main
  "Evolves vector of letters in alphabetical order."
  [& _]
  (println
    (ga/evolve {;; Generates random genomes as a permutation of the target genome.
                :genome-factory  #(shuffle target)
                ;; Phenomes are a map containing a scalar `:error` for the genome.
                ;; Additionally :errors is 0 or 1 for each character.
                ;; The `:genome` is added implicitly.
                :genome->phenome (fn [gn] 
                                   (let [errors (map #(if (= %1 %2) 0 1)
                                                     gn
                                                     target)]
                                     {:errors errors
                                      :error (apply + errors)}))
                ;; To "breed" a new genome from the population, we:
                ;;   1. Select 2 parents with lexicase selection.
                ;;   2. Pass their genomes to uniform-crossover.
                ;;   3. Mutate the resulting genome by swapping the position of 2 genes.
                :breed           (fn [population]
                                   (->> (repeatedly 2 #(lexicase population))
                                        (map :genome)
                                        tb/uniform-crossover
                                        tb/swap-2-genes))
                ;; We compare individuals on the basis of the error values. Lower is better.
                :phenome-cmp     (comparator #(< (:error %1) (:error %2)))
                ;; We stop evolution when either:
                ;;   1. We find an individual with zero error or
                ;;   2. We reach 300 generations.
                :stop-fn         (fn [{:keys [generation best]}]
                                   (cond
                                     (= (:error best) 0) :solution-found
                                     (= generation 300) :max-generation-reached))
                ;; Each generation will contain 1000 individuals.
                :population-size 1000}))
  (shutdown-agents))
