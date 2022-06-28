(ns erp12.ga-clj.search.ga
  "A simple generational genetic algorithm."
  (:require [erp12.ga-clj.individual :as i]
            [erp12.ga-clj.utils :as u]))

(defn- make-generation
  [{:keys [pre-generation population-factory post-generation context]
    :or   {pre-generation  (constantly nil)
           post-generation (constantly nil)
           context         {}}}]
  (let [pre-gen-data (or (pre-generation context) {})
        population (population-factory pre-gen-data)
        gen-data (merge context pre-gen-data {:population population})
        post-gen-data (or (post-generation gen-data) {})]
    (merge context gen-data post-gen-data)))

(defn- initial-generation
  [{:keys [genome-factory individual-factory population-size mapper]
    :or   {mapper map}
    :as   opts}]
  (make-generation
    (assoc opts
      :population-factory
      (fn [config]
        (mapper (fn [_] (i/make-individual individual-factory (genome-factory) config))
                (range population-size))))))

(defn- next-generation
  [{:keys [genomes individual-factory mapper]
    :or   {mapper map}
    :as   opts}]
  (make-generation
    (assoc opts
      :population-factory
      (fn [config]
        (mapper #(i/make-individual individual-factory % config) genomes)))))

(defn run
  "Run a generational genetic algorithm.

  Options:
    :genome-factory     - A nullary function to produce the (random) genomes of the initial population.
    :pre-generation     - A function that will be called before each generation. Used for side effects and deriving
                          data shared throughout the generation. More information below.
                          Default is a function which returns nil.
    :individual-factory - A function which converts a genome into a map, referred to as the \"individual\".
                          The :genome is added to the resulting map implicitly.
    :post-generation    - A unary function that will be called after each generation. Used to derive data accessible
                          when generating the next generation. Default is a function which returns nil.
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

  The `pre-generation` function must take a single \"context\" map argument and return either `nil` or a map.
  The context map passed to the `pre-generation` function contains the following entries:
    :step - The number of steps (aka generations) performed so far.
  If provided, the returned map will be available to all calls of `:individual-factory` throughout the current
  generation. This can be used to create once-per-generation configuration data, for example a subset of
  training cases to evaluate each genome.

  The `post-generation` function must take a single map and return either `nil` or a map. The map passed will
  contain the follow entries:
    :step - The number of steps (aka generations) performed so far.
    :population - The individuals (maps with :genome) of the generation just constructed.
    The entire contents of the map returned by `pre-generation`.
  If provided, the returned map will be available to all calls of `:breed:` (in addition to the population)
  to create the subsequent generation. This can be used to compute statistics about the entire generation that
  will drive breeding. For example, the median error value can be computed to produce a selection threshold.

  The `stop-fn` is used to determine if evolution should be stopped. It is passed a map containing the
  following options:
    :generation-number - The number of generations that have gone
    :generation        - The entire generation map, including the population.
    :best              - The best individual seen throughout evolution so far according to the :individual-cmp.
    :new-best?         - True if the :best individual is new this generation, false otherwise. Can be used
                         to skip expensive logic to determine if the solution is good enough.
  The `stop-fn` should return `nil` if evolution should continue. Otherwise, it must return a truthy value
  that will be included in the map returned by `evolve` under the key `:result`."
  [{:keys [genome-factory pre-generation individual-factory post-generation breed individual-cmp stop-fn population-size mapper]
    :or   {pre-generation  (constantly nil)
           post-generation (constantly nil)
           mapper          #?(:clj pmap :cljs map)}
    :as   opts}]
  {:pre [(some? genome-factory)
         (some? individual-factory)
         (some? individual-cmp)
         (some? breed)
         (some? stop-fn)
         (some? population-size)]}
  (let [opts (merge opts
                    ;; Put the default values into opts if not supplied.
                    {:pre-generation  pre-generation
                     :post-generation post-generation
                     :mapper          mapper})]
    (loop [generation (initial-generation (assoc opts :context {:step 0}))
           best-seen nil]
      (let [population (:population generation)
            ;; @todo Is there anyway to make this less expensive?
            new-best (u/min-by-cmp individual-cmp
                                   (if (nil? best-seen)
                                     population
                                     (conj population best-seen)))
            result (stop-fn (assoc generation
                              :best new-best
                              :new-best? (not= best-seen new-best)))]
        (if (some? result)
          {:step   (:step generation)
           :result result
           :best   new-best}
          (recur (let [new-genomes (doall (repeatedly population-size #(breed generation)))
                       new-context {:step (inc (:step generation))}]
                   (next-generation (assoc opts :context new-context :genomes new-genomes)))
                 new-best))))))
