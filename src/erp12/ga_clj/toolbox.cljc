(ns erp12.ga-clj.toolbox
  "Algorithms often used by genetic algorithms.

  Most functions fall into one of the following families of algorithms:
  - Error/loss functions
  - Parent selection
  - Mutation
  - Recombination

  Some functions make assumptions about the structure of genomes and/or the attributes of individuals.
  See function docstrings for more details.")

;; @todo Add tests for this whole file!

;; Errors / Loss

(defn hamming-distance
  [actual expected]
  (apply + (map #(if (= %1 %2) 0 1) actual expected)))

;; Selection

(defn make-tournament-selection
  [{:keys [by size]}]
  (fn [population]
    (apply min-key by (take size (shuffle population)))))


(defn lexicase-selection
  "Helper for make-lexicase-selection. Takes the population and cases shuffled
   in a random order. Behaves deterministically to aid in testing."
  [candidates cases]
  ;; Stop when cases is empty or candidates has only 1 left.
  (if (or (empty? cases)
          (= 1 (count candidates)))
    (rand-nth candidates)
    (let [the-case (first cases)
          best (apply min (map #(nth (:errors %) the-case)
                               candidates))]
      (recur (filter #(= best (nth (:errors %) the-case))
                     candidates)
             (rest cases)))))

;; @todo Implement epsilon lexicase
;; Not sure how where I would calculate epsilons just once per generation.
(defn epsilon-lexicase-selection
  "Implements semi-dynamic epsilon lexicase, which seems best in trial.
   semi-dynamic = use local best, but global epsilons that are calculated
   only once per generation."
  [candidates cases]
  :TODO
  )

;; @todo How to handle downsampled lexicase selection?
;;                       |--> downsampling I assume would happen wherever individuals
;;                            are evaluated, not here
;; @todo Pre-selection filtering of the population? Maybe only if using lexicase-selection?
(defn make-lexicase-selection
  "Applies lexicase selection to the population, returning a single individual."
  [{:keys [use-epsilon-lexicase]}]
  (fn [population]
    (let [cases (shuffle (range (count (:errors (first population)))))]
      (if use-epsilon-lexicase
        (lexicase-selection population
                            cases)
        (epsilon-lexicase-selection population
                                    cases)))))


;; Mutation

(defn swap-2-genes
  "Swaps the position of 2 genes in the given genome."
  [genome]
  (let [gn (vec genome)
        [idx1 idx2] (repeatedly 2 #(rand-int (count gn)))]
    (assoc gn idx2 (gn idx1) idx1 (gn idx2))))


(defn uniform-addition
  [genome addition-rate genetic-source]
  (apply concat
         (map #(if (< (rand) addition-rate)
                 (if (< (rand) 0.5)
                   (list % (rand-nth genetic-source))
                   (list (rand-nth genetic-source) %))
                 (list %))
              genome)))

(defn uniform-deletion
  [genome deletion-rate]
  (random-sample (- 1.0 deletion-rate) genome))

(defn umad
  "Performs uniform mutation by addition and deletion.
   First pass adds a new gene before or after each gene in the genome.
   Second pass deletes genes with some probability."
  [genome addition-rate deletion-rate genetic-source]
  (-> genome
      (uniform-addition addition-rate genetic-source)
      (uniform-deletion deletion-rate)))


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
