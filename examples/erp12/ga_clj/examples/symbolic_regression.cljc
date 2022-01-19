(ns erp12.ga-clj.examples.symbolic-regression
  "Symbolic regression to fit data generated from x^2 + 3x + 2 with a bit of noise added.
  Adapted from: Lee Spector (https://github.com/lspector/gp)"
  (:require [kixi.stats.math :as math]
            [kixi.stats.distribution :as dist]
            [erp12.ga-clj.generational :as ga]
            [erp12.ga-clj.toolbox :as tb]))

;; x^2 + 3x + 2 + noise
(def x-train (vec (range -20.0 20.0 0.05)))
(def y-train (mapv (fn [x noise]
                     (+ (+ (+ (* x x) (* 3 x)) 2)
                        noise))
                   x-train
                   ;; Add some noise to the data.
                   (dist/sample (count x-train) (dist/normal {:mu 0 :sd 0.1}))))

(defn p-div
  "Protected division."
  [n d]
  (if (zero? d)
    0
    (/ n d)))

(def fn->arity
  {+        2
   -        2
   *        2
   p-div    2
   math/sin 1
   math/cos 1})

(defn random-function
  []
  (rand-nth (keys fn->arity)))

(defn random-terminal
  []
  (rand-nth (list 'x (- (rand 5) 1))))

(defn random-code
  "Create a random tree of code."
  [depth]
  (if (or (zero? depth)
          (zero? (rand-int 2)))
    (random-terminal)
    (let [f (random-function)]
      (cons f (repeatedly (get fn->arity f)
                          #(random-code (dec depth)))))))

(def mutate
  ;; We mutate trees by replacing a random subtree with a random tree.
  (tb/make-subtree-mutation {:tree-generator #(random-code 2)}))

(def select
  ;; Creates a parent selection function that uses the lexicase selection
  ;; algorithm. We supply a function-live value to the `:epsilon` field
  ;; to specify how a value for epsilon will be pulled from the generation.
  ;; In this case, we will store a vector of per-case epsilon under the `:epsilon`
  ;; key of the generation map.
  (tb/make-lexicase-selection {:epsilon :epsilon})

  ;; To use traditional lexicase selection (no epsilon) we can specify `:epsilon` as
  ;; a falsey value, which is also the default.
  ; (tb/make-lexicase-selection)

  ;; We can also supply a static, constant, value for epsilon.
  ; (tb/make-lexicase-selection {:epsilon 0.1})
  )

(defn -main
  [& _]
  (println
    (ga/evolve {;; Creates a random tree, corresponding to an equation, with a max depth of 3.
                :genome-factory     #(random-code 3)
                ;; Before each generation (evaluation), randomly select 5% of the training
                ;; cases to use for evaluation. All genomes in the generation will be evaluated
                ;; on these same cases.
                :pre-generation     (fn [{:keys [step]}]
                                      {:batch-cases (random-sample 0.05 (range (count x-train)))})
                ;; Individuals are a maps containing
                ;;   1. A `:model` represented as a callable Clojure funciton.
                ;;   2. A vector of predictions, stored under `:y-pred`.
                ;;   3. A vector of `:errors`, one for each training case in this generation's batch.
                ;;   4. The mean error across all cases, stored under `:mae`.
                ;;   5. The `:genome` tree which created the model. This is added implicitly.
                :individual-factory (fn [gn {:keys [batch-cases]}]
                                      (let [model (eval `(fn ~(vector 'x) ~gn))
                                            x-batch (mapv #(nth x-train %) batch-cases)
                                            y-batch (mapv #(nth y-train %) batch-cases)
                                            y-pred (mapv model x-batch)
                                            errors (mapv #(Math/abs (- %1 %2)) y-pred y-batch)]
                                        {:model  model
                                         :y-pred y-pred
                                         :errors errors
                                         :mae    (tb/mean errors)}))
                ;; After each generation is evaluated, compute a vector of `:epsilon` values
                ;; to use in parent selection. In this case, we will use the default computation
                ;; of epsilon: the median absolute deviation.
                :post-generation    (fn [{:keys [population] :as opts}]
                                      {:epsilon (tb/compute-epsilon-per-case population)})
                ;; To "breed" a new genome from the population, we:
                ;;   1. Select 2 parents with lexicsae selection. This will look-up
                ;;   2. Pass their genomes to subtree crossover.
                ;;   3. Mutate the resulting genome by with subtree mutation.
                :breed              (fn [generation]
                                      (->> (repeatedly 2 #(select generation))
                                           (map :genome)
                                           (apply tb/subtree-crossover)
                                           mutate))
                ;; We compare individuals on the basis of their mean absolute error. Lower is better.
                :individual-cmp     (comparator #(and (< (:mae %1) (:mae %2))
                                                      (not (math/infinite? (:mae %1)))))
                ;; We stop evolution when either:
                ;;   1. We reach 300 generations.
                ;;   2. We find an individual with zero MAE on the entire dataset.
                :stop-fn            (fn [{:keys [step best new-best?]}]
                                      (println "Step:" step
                                               "Best MAE:" (:mae best)
                                               "Best Tree Size:" (tb/tree-size (:genome best))
                                               "Best Tree Depth:" (tb/tree-depth (:genome best)))
                                      (cond
                                        ;; Stop evolution after 300 generations.
                                        (= step 300) :max-generation-reached
                                        ;; If a new "best" individual is found (based on MEA of a batch)
                                        ;; Test the new best individual on the full training set.
                                        ;; If the full MAE is below 0.3, report that the solution is found
                                        new-best? (let [y-pred (mapv (:model best) x-train)
                                                        mae (tb/mae y-pred y-train)]
                                                    (when (<= mae 0.2)
                                                      :solution-found))))
                ;; Each generation will contain 1000 individuals.
                :population-size    1000}))
  (shutdown-agents))
