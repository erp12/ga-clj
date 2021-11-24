(ns erp12.ga-clj.generational
  "A simple generational genetic algorithm."
  (:require [erp12.ga-clj.individual :as i]
            [erp12.ga-clj.utils :as u]))

;; @todo Allow extra per-generation data? For example, which cases used for downsampled-lexicase.

(defn evolve
  "Run a generational genetic algorithm.

  The number of individuals in each generation is given by `population-size`.

  The genomes of the initial population are generated via the nullary `genome-factory` function.

  Genomes are translated into individuals by calling the `genome->phenome` function, which must
  a map. The genome will be added to the map automatically.

  The `finalization` takes the population of individuals (post `genome->phenome`) and is expected to
  return a population of (possibly transformed) individuals. This function can be used to enhance
  individuals with attributes that require access to other individuals, filter the population,
  compute population statistics, log progress, and perform arbitrary side effects once per generation.

  The `phenome-cmp` is a comparator function that can be used to compare 2 individuals. It is used to track
  the best individual seen throughout evolution.

  The `stop-fn` is used to determine if evolution should be stopped. It is passed a map containing the
  generation number (`:generation`), current population (`:population`), and best individual seen during
  evolution so far (`:best`). The `stop-fn` should return `nil` if evolution should continue. Otherwise,
  it can return a truthy value that will be included in the map returned by `evolve` under the key `:result`.

  The `mapper` can be any function that implements a map-like higher order function.
  Common values are `map`, `pmap`, and `mapv`. The default is `pmap` for CLojure and `map` for Clojurescript.

  The `evolve` function will return a map with the following entries:
    :generation - The generation number after which evolution stopped.
    :result     - The truthy value returned by the `stop-fn`.
    :best       - The best individual seen throughout evolution.

  "
  [{:keys [genome-factory genome->phenome finalization breed phenome-cmp stop-fn population-size mapper]
    :or   {finalization vec
           mapper       #?(:clj pmap :cljs map)}
    :as   opts}]
  ;; @todo Is there a more informative name than `finalization`?
  {:pre [(some? genome-factory)
         (some? genome->phenome)
         (some? phenome-cmp)
         (some? breed)
         (some? stop-fn)
         (some? population-size)]}
  (loop [generation 0
         population (finalization (i/random-population opts))
         best-seen nil]
    (let [new-best (u/min-by-cmp phenome-cmp
                                 (if (nil? best-seen)
                                   population
                                   (conj population best-seen)))
          result (stop-fn {:generation generation
                           :population population
                           :best       new-best})]
      (if (some? result)
        {:generation generation
         :result     result
         :best       new-best}
        (recur (inc generation)
               ;; @todo Use with-error-context to display the context of any errors thrown during selection and variation.
               (->> (repeatedly population-size #(breed population))
                    (mapper #(i/genome->individual (:genome->phenome opts) %))
                    finalization)
               new-best)))))
