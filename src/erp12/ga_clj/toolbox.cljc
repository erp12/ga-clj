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

;; @todo How to handle downsampled lexicase selection? Where do cases come from?
(defn make-lexicase-selection
  [{:keys [epsilon]}]
  (fn [population]
    ;; @todo Write me!
    ))

;; Mutation

(defn swap-2-genes
  "Swaps the position of 2 genes in the given genome."
  [genome]
  (let [gn (vec genome)
        [idx1 idx2] (repeatedly 2 #(rand-int (count gn)))]
    (assoc gn idx2 (gn idx1) idx1 (gn idx2))))


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
