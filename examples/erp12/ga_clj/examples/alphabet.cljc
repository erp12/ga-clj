(ns erp12.ga-clj.examples.alphabet
  (:gen-class)
  (:require [erp12.ga-clj.generational :as ga]
            [erp12.ga-clj.toolbox :as tb]))

(def target
  (vec "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(def tournament
  (tb/make-tournament-selection {:by :error :size 7}))

(defn -main
  [& _]
  (println
    (ga/evolve {:genome-factory  #(shuffle target)
                :genome->phenome (fn [gn] {:error (tb/hamming-distance gn target)})
                :breed           (fn [population]
                                   (->> (repeatedly 2 #(tournament population))
                                        (map :genome)
                                        tb/uniform-crossover
                                        tb/swap-2-genes))
                :phenome-cmp     (comparator #(< (:error %1) (:error %2)))
                :stop-fn         (fn [{:keys [generation best]}]
                                   (cond
                                     (= (:error best) 0) :solution-found
                                     (= generation 300) :max-generation-reached))
                :population-size 1000}))
  (shutdown-agents))
