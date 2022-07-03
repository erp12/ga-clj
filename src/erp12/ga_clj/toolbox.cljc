(ns erp12.ga-clj.toolbox
  "Algorithms often used by genetic algorithms.

  Most functions fall into one of the following families of algorithms:
   - Error/loss functions
   - Parent selection
   - Mutation
   - Recombination

  Some functions make assumptions about the structure of genomes and/or the attributes of individuals.
  See function docstrings for more details."
  (:require [erp12.ga-clj.utils :as u]
            [kixi.stats.core :as stat]
            [kixi.stats.math :as math]))

;; Errors / Loss

(defn mean
  "Arithmetic mean."
  [arr]
  (transduce identity stat/mean arr))

(defn mae
  "Mean absolute error."
  [y-true y-pred]
  (transduce identity stat/mean (map #(math/abs (- %1 %2)) y-true y-pred)))

(defn mse
  "Mean squared error."
  [y-true y-pred]
  (transduce identity (stat/mse first second) (map vector y-pred y-true)))

(defn rmse
  "Root mean squared error."
  [y-true y-pred]
  (transduce identity (stat/rmse first second) (map vector y-pred y-true)))

(defn hamming-distance
  [actual expected]
  (apply + (map #(if (= %1 %2) 0 1) actual expected)))

(defn stdev
  "The sample standard deviation."
  [arr]
  (transduce identity stat/standard-deviation arr))

(defn median
  [coll]
  "The median."
  ;; Source: https://github.com/clojure-cookbook/clojure-cookbook/blob/master/01_primitive-data/1-20_simple-statistics.asciidoc
  ;; We don't use the kixi.stat.math implementation because it is CLJ only.
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (mean [bottom-val top-val])))))

(defn mad
  "Median absolute deviation (MAD)"
  [arr]
  (let [med (median arr)
        dev (map #(math/abs (- % med)) arr)]
    (median dev)))

;; Selection

(defn make-tournament-selection
  "Creates a tournament selection function for selecting individuals from a generation's population.
   Randomly samples a subset (aka tournament) of the population and selects the \"best\" individual
   from within the subset.

  The argument map requires the following keys:
    :size - The number of individuals in the tournament.
    :by   - A function applied to each individual. The return value should be comparable, and is
            typically a number representing the error of the individual. The individual in the tournament
            with the lowest value will be selected.

  The selection function returned by `make-tournament-selection` will leverage the `:population` field
  of a generation.

  See: Section 2.3 of the field guide to genetic programming.
  http://www0.cs.ucl.ac.uk/staff/W.Langdon/ftp/papers/poli08_fieldguide.pdf
  "
  [{:keys [by size]}]
  (fn [{:keys [population]}]
    (apply min-key by (take size (shuffle population)))))

(defn compute-epsilon-per-case
  "Computes a vector of epsilon values for epsilon lexicase selection based on the
  error vectors of a collection of individuals.

  Options:
    :errors-fn - The function that will extract the error vector from the individual. Default is :errors.
    :agg-by    - A aggregation function from all the error values of a single training case to a single
                 value of epsilon. Default is the median absolute deviation (mad)."
  ([individuals]
   (compute-epsilon-per-case individuals {}))
  ([individuals {:keys [errors-fn agg-by]
                 :or   {errors-fn :errors agg-by mad}}]
   (->> individuals
        (map errors-fn)
        (apply (partial map vector))
        (mapv agg-by))))

(defn- get-epsilon
  [epsilon context]
  (cond
    (or (number? epsilon) (indexed? epsilon)) epsilon
    (ifn? epsilon) (get-epsilon (epsilon context) {})
    (or (false? epsilon) (nil? epsilon)) 0
    :else (throw (ex-info "Failed to compute usable epsilon."
                          {:epsilon epsilon
                           :context context}))))

(defn lexicase-selection
  "Performs lexicase selection.

  Options:
    :candidates - A collection of individuals to select from.
    :errors-fn  - A function to call on individuals to return their error vectors.
    :cases      - A sequence of case (or error) indices. Typically, a randomly ordered seq
                  from 0 to the length of each individual's error vector.
    :epsilon    - Default is zero. See below for details.

  The :epsilon option controls how to compute the value for epsilon.  A scalar number will
  be used the epsilon across all cases. An indexed collection of numbers will use each
  element as the epsilon for the corresponding index of the error vectors.

  Citations:
    https://arxiv.org/abs/2106.06085
    https://arxiv.org/abs/1905.09372
    https://arxiv.org/abs/1905.13266
    https://arxiv.org/abs/1709.05394"
  [{:keys [candidates errors-fn cases epsilon] :or {epsilon 0} :as opts}]
  (if (or (empty? cases)
          (= 1 (count candidates)))
    (rand-nth candidates)
    (let [the-case (first cases)
          get-error #(nth (errors-fn %) the-case)
          epsilon-this-case (if (indexed? epsilon)
                              (nth epsilon the-case)
                              epsilon)
          threshold (+ (apply min (map get-error candidates)) epsilon-this-case)]
      (recur (assoc opts
               :candidates (filter #(<= (get-error %) threshold) candidates)
               :cases (rest cases))))))

(defn make-lexicase-selection
  "Creates a selection function for performing lexicase selection. See`lexicase-selection`.\n

  Options:
    :errors-fn - The function that will extract the error vector from the individual.
                 Default is :errors.
    :epsilon   - The value for epsilon, or a strategy for computing epsilon. Default is nil.
                 See below for details.

  The :epsilon option can be
    - A falsey value. This will result in an epsion of 0, aka traditional lexicase selection.
    - A scalar number.  This will be used the epsilon across all cases
    - An indexed collection of numbers. Each element will be the epsilon for the corresponding
      index of the individuals' error vectors.
    - A function (specifically ifn?). ...

  Citations:
    https://arxiv.org/abs/2106.06085
    https://arxiv.org/abs/1905.09372
    https://arxiv.org/abs/1905.13266
    https://arxiv.org/abs/1709.05394"
  ([]
   (make-lexicase-selection {}))
  ([{:keys [errors-fn epsilon] :or {errors-fn :errors}}]
   (fn [{:keys [population] :as generation}]
     (lexicase-selection {:candidates (if (not epsilon)
                                        ;; @todo If epsilon is pre-computed, why can't we distinct the candidates by their errors?
                                        (u/random-distinct-by errors-fn population)
                                        population)
                          :errors-fn  errors-fn
                          :cases      (let [;; Take the min number of cases.
                                            ;; Sometimes error vectors are padded in edge cases where penalties are
                                            ;; applied without knowledge of the number of cases.
                                            ;; @todo Reconsider if this is a good idea.
                                            num-cases (->> population (map #(count (errors-fn %))) (reduce min))]
                                        (shuffle (range num-cases)))
                          :epsilon    (get-epsilon epsilon (dissoc generation :population))}))))

;; Mutation

(defn swap-2-genes
  "Swaps the position of 2 genes in the given sequential genome."
  [genome]
  (let [gn (vec genome)
        [idx1 idx2] (repeatedly 2 #(rand-int (count gn)))]
    (assoc gn idx2 (gn idx1) idx1 (gn idx2))))

(defn make-uniform-addition
  "Creates a mutation function that uniformly randomly adds genes to a sequential genome.

  Options:
    :addition-rate  - The probability of adding a gene at any particular location.
    :genetic-source - A 0-arg function that returns a (random) gene when called."
  [{:keys [addition-rate genetic-source]}]
  (fn [genome]
    (mapcat #(if (< (rand) addition-rate)
               (if (< (rand) 0.5)
                 (list % (genetic-source))
                 (list (genetic-source) %))
               (list %))
            genome)))

(defn make-uniform-deletion
  "Creates a mutation function that uniformly randomly removes genes from a sequential genome.

  Options:
    :deletion-rate  - The probability of removing each gene."
  [{:keys [deletion-rate]}]
  (fn [genome] (random-sample (- 1.0 deletion-rate) genome)))

(defn make-umad
  "Creates a mutation function that performs \"Uniform Mutation by Addition and Deletion\"
  on a sequential, linear genome.

  Options:
    :addition-rate  - The probability of adding a gene at any particular location.
    :deletion-rate  - The probability of removing each gene.
    :genetic-source - A 0-arg function that returns a (random) gene when called.

  Citation:
    https://dl.acm.org/doi/10.1145/3205455.3205603"
  [opts]
  (comp (make-uniform-deletion opts)
        (make-uniform-addition opts)))

(defn make-size-neutral-umad
  "Creates a mutation function that performs \"Uniform Mutation by Addition and Deletion\"
  on a sequential, linear genome such that the addition and deletion rates are balanced.
  The average change in genome length will be zero.

  Options:
    :rate           - The probability of adding a gene at any particular location.
    :genetic-source - A 0-arg function that returns a (random) gene when called.

  Citation:
    https://dl.acm.org/doi/10.1145/3205455.3205603"
  [{:keys [rate] :as opts}]
  (make-umad (merge opts
                    {:addition-rate rate
                     :deletion-rate (/ rate (+ 1 rate))})))

(defn tree-size
  [tree]
  (if (sequential? tree)
    (count (flatten tree))
    1))

(defn tree-depth
  [tree]
  (if (sequential? tree)
    (if (empty? tree)
      0
      (inc (apply max (map tree-depth tree))))
    0))

(defn replace-subtree
  [tree position replacement]
  (cond
    (zero? position) replacement

    (sequential? tree)
    (->> tree
         (reduce (fn [[offset acc] sub-tree]
                   [(+ offset (tree-size sub-tree))
                    (conj acc (replace-subtree sub-tree (- position offset) replacement))])
                 [0 (empty tree)])
         second
         ;; Corrects order for prepend collections.
         (into (empty tree)))

    :else tree))

(defn replace-random-subtree
  [tree replacement]
  (replace-subtree tree (rand-int (tree-size tree)) replacement))

(defn make-subtree-mutation
  [{:keys [tree-generator]}]
  (fn [tree]
    (replace-random-subtree tree (tree-generator))))

;; Recombination

(defn uniform-crossover
  [genomes]
  (loop [child-gn []
         idx 0]
    (let [gene (nth (rand-nth genomes) idx nil)]
      (if (nil? gene)
        child-gn
        (recur (conj child-gn gene)
               (inc idx))))))

;; @todo n-point-crossover
;; @todo 1-point-crossover
;; @todo 2-point-crossover

(defn random-subtree
  [tree]
  (if (zero? (rand-int (tree-size tree)))
    tree
    (recur (rand-nth (rest tree)))))

(defn subtree-crossover
  [tree-a tree-b]
  (replace-random-subtree tree-a (random-subtree tree-b)))
