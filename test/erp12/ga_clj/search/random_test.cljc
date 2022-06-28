(ns erp12.ga-clj.search.random-test
  (:require [clojure.test :refer [deftest is]]
            [erp12.ga-clj.search.random :refer [run]]))

(deftest run-test
  (let [target [1 2 3 4]]
    (is (= {:result     :solution-found
            :individual {:genome target}}
           (dissoc
             (run {:genome-factory     #(shuffle target)
                   :individual-factory (fn [gn _] {:genome gn})
                   :stop-fn            (fn [{:keys [step individual]}]
                                         ;(when (zero? (mod step 10))
                                         ;  (println "Step:" step "Individual:" individual))
                                         (cond
                                           (= (:genome individual) target) :solution-found
                                           (= step 1000) :max-step-reached))})
             :step)))))
