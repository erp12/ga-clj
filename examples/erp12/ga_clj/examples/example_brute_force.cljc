(ns erp12.ga-clj.examples.example-brute-force
  (:gen-class)
  (:require [erp12.ga-clj.search.brute-force :as bf]))

(defn -main
  "Tests brute force on a toy problem.
   genetic-source individual-factory stop-fn individual-cmp max-steps"
  [& _]
  (let [best-ind (atom {:error 1000000000})]
    (println
     (bf/run {;; Genetic source contains the possible genes
              :genetic-source '(:A :B :C :D :E)

             ;; Individuals are a map containing a scalar `:error` for the genome.
             ;; Error is abs(number of :E minus number of :B) + abs(4 - number of :E)
             ;; + number of :A
              :individual-factory (fn [gn _]
                                    (let [freqs (frequencies gn)
                                          numA (get freqs :A 0)
                                          numE (get freqs :E 0)
                                          numB (get freqs :B 0)
                                          error (+ (abs (- numE numB))
                                                   (* 10 (abs (- 3 numE)))
                                                   (* 5 (abs (- 2 numA))))]
                                      {:error error}))

             ;; We stop evolution when either:
             ;;   1. We find an individual with zero error or
             ;;   2. We reach max-steps steps
              :stop-fn            (fn [{:keys [step individual max-steps]}]
                                    (when (< (:error individual) (:error @best-ind))
                                      (reset! best-ind individual)
                                      (println "Step:" step "\tBest:" individual))
                                    (cond
                                      (= (:error individual) 0) :solution-found
                                      (>= step max-steps) :max-step-reached))

             ;; Maximum number of brute force steps
              :max-steps          300000}))))


(comment
  
  (-main)
  
  )