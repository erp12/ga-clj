(ns erp12.ga-clj.individual
  "Functions for creating and transforming individuals.

  An individual is a map with (at least) the keyword `:genome`.
  Typically, individuals will contain other attributes such as data structures representing
  the fitness/error of the individual."
  #?(:clj (:require [erp12.ga-clj.utils :refer [with-error-context]])
     :cljs (:require-macros [erp12.ga-clj.utils :refer [with-error-context]])))

(defn genome->individual
  "Builds an individual (a map containing genome and phenome data) from a genome.
  The `genome->phenome` must return a map.

  If an exception is raised while building the individual, it will be wrapped
  in an outer exception that includes the genome in the `ex-data` to aid in
  debugging."
  [genome->phenome genome]
  (with-error-context
    {:msg    "Failed to create phenome from genome."
     :genome genome}
    (assoc (genome->phenome genome) :genome genome)))

(defn random-population
  "Creates a population of individuals from random genomes.

  Genomes are created with the nullary `genome-factory` function.
  Each genome is converted into an individual using the `genome->phenome`
  function via `(genome->individual)`.

  The `mapper` can be any function that implements a map-like higher order
  function. Common values are `map`, `pmap`, and `mapv`. The default is `map`.
  "
  [{:keys [genome-factory genome->phenome population-size mapper]
    :or   {mapper map}}]
  (let [make-random-individual (comp (partial genome->individual genome->phenome) genome-factory)]
    (mapper (fn [_] (make-random-individual)) (range population-size))))
