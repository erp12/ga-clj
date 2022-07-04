(ns erp12.ga-clj.search.simulated-annealing
  "Simulated Annealing"
  (:require [erp12.ga-clj.individual :as i]))

(defn- with-error
  [indiv error-fn]
  (assoc indiv ::error (error-fn indiv)))

(defn run
  [{:keys [genome-factory individual-factory stop-fn error-fn mutate max-steps]}]
  (loop [step 0
         best (with-error (i/make-individual individual-factory (genome-factory)) error-fn)]
    (let [;; Create neighbor through mutation.
          neighbor (with-error (i/make-individual individual-factory (mutate best)) error-fn)
          ;; Compute the temperature at this step.
          temp (- 1.0 (/ step max-steps))
          ;; If neighbor is better than current best individual, keep it.
          ;; Otherwise, keep the neighbor with some probability according
          ;; to the temperature and how much worse the neighbor is.
          accept? (if (< (::error neighbor) (::error best))
                    true
                    (< (rand)
                       (Math/exp (/ (- (- (::error neighbor) (::error best)))
                                    temp))))
          result (stop-fn {:step            step
                           :individual      neighbor
                           :temp            temp
                           :max-steps       max-steps
                           :new-individual? accept?})]
      (if (some? result)
        {:step   step
         :result result
         :best   (if (< (::error neighbor) (::error best))
                   neighbor
                   best)}
        (recur (inc step)
               (if accept? neighbor best))))))