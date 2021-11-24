(ns erp12.ga-clj.individual-test
  (:require [clojure.test :refer [deftest is testing]]
            [erp12.ga-clj.individual :refer [genome->individual random-population]])
  #?(:clj (:import (clojure.lang ExceptionInfo))))


(deftest genome->individual-test
  (is (= (genome->individual (fn [genome]
                               {:error-vector [0.9 1.0 0.0 0.5 0.1]
                                :total-error  2.5
                                :losses       {:mse 1.1 :mae 0.75}})
                             "gtattccgcgtcggga")
         {:genome       "gtattccgcgtcggga"
          :error-vector [0.9 1.0 0.0 0.5 0.1]
          :total-error  2.5
          :losses       {:mse 1.1 :mae 0.75}}))
  (testing "Error in genome->phenome"
    (let [e (try
              (genome->individual (fn [genome] {:bad (count 0)}) "gtattccgcgtcggga")
              nil
              (catch #?(:clj ExceptionInfo :cljs js/Error) e
                e))]
      (is (= (ex-message e) "Failed to create phenome from genome."))
      (is (= (ex-data e) {:genome "gtattccgcgtcggga"}))
      (is (instance? #?(:clj UnsupportedOperationException :cljs js/Error)
                     (ex-cause e)))))
  (testing "Non-map phenome"
    (let [e (try
              (genome->individual (fn [_] 0) "gtattccgcgtcggga")
              nil
              (catch #?(:clj ExceptionInfo :cljs js/Error) e
                e))]
      (is (= (ex-message e) "Failed to create phenome from genome."))
      (is (= (ex-data e) {:genome "gtattccgcgtcggga"}))
      (is (instance? #?(:clj ClassCastException :cljs js/Error)
                     (ex-cause e))))))


(deftest random-population-test
  (let [dummy-genome {:value + :children [{:value 1} {:value 2}]}
        pop (random-population {:genome-factory  (constantly dummy-genome)
                                :genome->phenome (fn [genome]
                                                   {:depth       2
                                                    :errors      [3 7]
                                                    :total-error 10})
                                :population-size 3})]
    (is (= (count pop) 3))
    (doseq [individual pop]
      (is (= individual
             {:genome      dummy-genome
              :depth       2
              :errors      [3 7]
              :total-error 10})))))
