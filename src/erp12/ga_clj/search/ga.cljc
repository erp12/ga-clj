(ns erp12.ga-clj.search.ga
  (:require [erp12.ga-clj.individual :as i]
            [erp12.ga-clj.utils :as u]))

(defn- init-state
  [{:keys [genome-factory population-size mapper]}]
  {:step    0
   :genomes (mapper (fn [_] (genome-factory)) (range population-size))})

(defn- eval-generation
  [{:keys [genomes] :as state}
   {:keys [pre-eval evaluator post-eval mapper]}]
  (let [state' (-> state
                   (merge (pre-eval state))
                   (dissoc :genomes))
        individuals (mapper (fn [gn] (i/make-individual evaluator gn state')) genomes)
        state' (assoc state' :individuals individuals)]
    (merge state' (post-eval state'))))

(defn- next-generation
  [{:keys [step] :as state} {:keys [breed population-size mapper]}]
  {:step    (inc step)
   :genomes (mapper (fn [_] (breed state)) (range population-size))})

(defn run
  "Run a generational genetic algorithm.

  Options:
    :genome-factory     - A nullary function to produce the (random) genomes of the initial population.
    :pre-eval           - A function that will be called once per generation before evaluation. Must return either
                          nil or a map to be merged into the current state. Used for side effects and computing
                          data used by evaluation. More information below. Default is a function which returns nil.
    :evaluation         - A function which converts a genome into a map, referred to as the \"individual\".
                          The :genome is added to the resulting map implicitly.
    :post-eval          - A function that will be called once per generation after evaluation. Must return either
                          nil or a map to be merged into the current state. Used for side effects and computing
                          data used by breeding. More information below. Default is a function which returns nil.
    :breed              - A function for creating a new \"child\" genome. Must take a map containing the
                          state of evolution (containing at least an :individuals key) and return a genome.
                          This is where selection and variation occurs.
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

  The `pre-eval` function must take a single \"state\" map argument and return either `nil` or a map. The
  returned map will be merged into the state and made available during evaluation. Common use cases for `pre-eval`
  include deriving a sample of random training cases.

  The `post-eval` function must take a single \"state\" map argument and return either `nil` or a map. The
  returned map will be merged into the state and made available during breeding. This can be used to compute
  statistics about the entire generation that will drive breeding. For example, the median error value can be
   computed to produce a selection threshold. The `post-eval` function will always receive the following keys:
    :step - The number of steps (aka generations) performed so far.
    :individuals - The individuals (maps with :genome) of the generation that was just evaluated.

  The `stop-fn` is used to determine if evolution should be stopped. It is passed the \"state\" map containing
  (at least) the following entries along with any entries from `pre-eval` and `post-eval`:
    :step - The number of generations that have gone
    :generation        - The entire generation map, including the population.
    :best              - The best individual seen throughout evolution so far according to the :individual-cmp.
    :new-best?         - True if the :best individual is new this generation, false otherwise. Can be used
                         to skip expensive logic to determine if the solution is good enough.
  The `stop-fn` must return a falsey value if evolution should continue. Otherwise, it must return a truthy value
  that will be included in the map returned by `evolve` under the key `:result`."
  [{:keys [genome-factory pre-eval evaluator post-eval breed individual-cmp stop-fn population-size mapper]
    :or   {pre-eval  (constantly nil)
           post-eval (constantly nil)
           mapper    #?(:clj pmap :cljs map)}
    :as   opts}]
  {:pre [(some? genome-factory)
         (some? evaluator)
         (some? breed)
         (some? population-size)]}
  (let [opts (merge opts
                    ;; Put the default values into opts if not supplied.
                    {:pre-eval  pre-eval
                     :post-eval post-eval
                     :mapper    mapper})]
    (loop [state (init-state opts)
           best-seen nil]
      (let [state' (eval-generation state opts)
            ;; @todo Is there anyway to make this less expensive?
            new-best (u/min-by-cmp individual-cmp
                                   (if (nil? best-seen)
                                     (:individuals state')
                                     (conj (:individuals state') best-seen)))
            result (stop-fn (assoc state'
                              :best new-best
                              :new-best? (not= best-seen new-best)))]
        (if (some? result)
          {:step   (:step state')
           :result result
           :best   new-best}
          (recur (next-generation state' opts)
                 new-best))))))

