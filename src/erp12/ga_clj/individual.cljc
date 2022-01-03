(ns erp12.ga-clj.individual
  "Functions for creating and transforming individuals.

   An individual is a map with (at least) the keyword `:genome`. Typically, individuals will
   contain other attributes such as the fitness/error of the individual."
  #?(:clj  (:require [erp12.ga-clj.utils :refer [with-error-context]])
     :cljs (:require-macros [erp12.ga-clj.utils :refer [with-error-context]])))

(defn make-individual
  "Builds an individual (a map containing a genome and other data).

   The `genome->individual` function must return a map. The map does not need to contain
   the genome because it will be implicitly added under the `:genome` key.

   If an exception is raised while building the individual, it will be wrapped
   in an outer exception that includes the genome in the `ex-data` to aid in
   debugging."
  [genome->individual genome opts]
  (with-error-context
    {:msg    "Failed to create individual from genome."
     :genome genome
     :opts   opts}
    (assoc (genome->individual genome opts) :genome genome)))
