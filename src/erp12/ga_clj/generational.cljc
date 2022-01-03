(ns erp12.ga-clj.generational
  "A simple generational genetic algorithm."
  (:require [erp12.ga-clj.individual :as i]
            [erp12.ga-clj.utils :as u])
  #?(:clj  (:require [erp12.ga-clj.utils :refer [with-error-context]])
     :cljs (:require-macros [erp12.ga-clj.utils :refer [with-error-context]])))

(defn- make-generation
  [{:keys [pre-generation population-factory post-generation] :as opts}]
  (let [m1 (pre-generation)
        population (population-factory m1)
        m2 (post-generation population)]
    (merge m1 {:population population} m2)))

(defn- initial-generation
  [{:keys [genome-factory genome->individual population-size mapper]
    :or   {mapper map}
    :as   opts}]
  (make-generation
    (assoc opts
           :population-factory
           (fn [config]
             (mapper (fn [_] (i/make-individual genome->individual (genome-factory) config))
                     (range population-size))))))

(defn- next-generation
  [{:keys [genomes genome->individual mapper]
    :or   {mapper map}
    :as   opts}]
  (make-generation
    (assoc opts
           :population-factory
           (fn [config]
             (mapper #(i/make-individual genome->individual % config) genomes)))))

(defn evolve
  "Run a generational genetic algorithm.

  Options:
    :genome-factory     - A nullary function to produce the (random) genomes of the initial population.
    :pre-generation     - A nullary function that will be called before each generation. Must return a map.
                          The resulting data will be available to all calls of `:genome->individual`
                          throughout the generation. Default is a function which returns an empty map.
    :genome->individual - A function which converts a genome into a map, referred to as the \"individual\".
                          The :genome is added to the resulting map implicitly.
    :post-generation    - A unary function that will be called before each generation. Must take a collection
                          of individuals (the population of the previous generation) and return a map.
                          The resulting data will be available to all calls of `:breed` used to create the
                          subsequent generation. Default is a function which returns ae empty map.
    :breed              - A function for creating a new \"child\" genome. Must take a map containing the
                          data of the previous generation (containing at least a :population key) and
                          return a genome. This is where selection and variation must occur.
    :individual-cmp     - A comparator function that can be used to compare 2 individuals. It is used to
                          track the best individual seen throughout evolution.
    :stop-fn            - A functions used to determine if evolution should be stopped. Details below.
    :population-size    - The number of individuals in each generation.
    :mapper             - A map-like higher order function. Common values are `map`, `pmap`, and `mapv`.
                          The default is `pmap` for clj and `map` for cljs.

  The `evolve` function will return a map with the following entries:
    :generation - The generation number after which evolution stopped.
    :result     - The truthy value returned by the `stop-fn`.
    :best       - The best individual seen throughout evolution.

  The `stop-fn` is used to determine if evolution should be stopped. It is passed a map containing the
  following options:
    :generation-number - The number of generations that have gone
    :generation        - The entire generation map, including the population.
    :best              - The best individual seen throughout evolution so far according to the :individual-cmp.
    :new-best?         - True if the :best individual is new this generation, false otherwise. Can be used
                         to skip expensive logic to determine if the solution is good enough.
  The `stop-fn` should return `nil` if evolution should continue. Otherwise, it must return a truthy value
  that will be included in the map returned by `evolve` under the key `:result`."
  [{:keys [genome-factory pre-generation genome->individual post-generation breed individual-cmp stop-fn population-size mapper]
    :or   {pre-generation  hash-map
           post-generation (fn [_] {})
           mapper          #?(:clj pmap :cljs map)}
    :as   opts}]
  {:pre [(some? genome-factory)
         (some? genome->individual)
         (some? individual-cmp)
         (some? breed)
         (some? stop-fn)
         (some? population-size)]}
  (let [opts (merge opts
                    ;; Put the default values into opts if not supplied.
                    {:pre-generation  pre-generation
                     :post-generation post-generation
                     :mapper          mapper})]
    (loop [generation-number 0
           generation (initial-generation opts)
           best-seen nil]
      (let [population (:population generation)
            new-best (u/min-by-cmp individual-cmp
                                   (if (nil? best-seen)
                                     population
                                     (conj population best-seen)))
            result (stop-fn {:generation-number generation-number
                             :generation        generation
                             :best              new-best
                             :new-best?         (not= best-seen new-best)})]
        (if (some? result)
          {:generation generation-number
           :result     result
           :best       new-best}
          (recur (inc generation-number)
                 (next-generation (assoc opts :genomes (repeatedly population-size #(breed generation))))
                 new-best))))))
