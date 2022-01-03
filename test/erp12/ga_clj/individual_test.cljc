(ns erp12.ga-clj.individual-test
  (:require [clojure.test :refer [deftest is testing]]
            [erp12.ga-clj.individual :refer [make-individual]])
  #?(:clj (:import (clojure.lang ExceptionInfo))))


(deftest make-individual-test
  (is (= (make-individual (fn [genome opts]
                               {:error-vector [0.9 1.0 0.0 0.5 0.1]
                                :total-error  2.5
                                :losses       {:mse 1.1 :mae 0.75}})
                          "gtattccgcgtcggga"
                          {})
         {:genome       "gtattccgcgtcggga"
          :error-vector [0.9 1.0 0.0 0.5 0.1]
          :total-error  2.5
          :losses       {:mse 1.1 :mae 0.75}}))
  (testing "Exception raised in make-individual"
    (let [e (try
              (make-individual (fn [_ _] {:bad (count 0)}) "gtattccgcgtcggga" {})
              nil
              (catch #?(:clj ExceptionInfo :cljs js/Error) e
                e))]
      (is (= (ex-message e) "Failed to create individual from genome."))
      (is (= (ex-data e) {:genome "gtattccgcgtcggga"
                          :opts {}}))
      (is (instance? #?(:clj UnsupportedOperationException :cljs js/Error)
                     (ex-cause e)))))
  (testing "Non-map phenome"
    (let [e (try
              (make-individual (fn [_ _] 0) "gtattccgcgtcggga" {})
              nil
              (catch #?(:clj ExceptionInfo :cljs js/Error) e
                e))]
      (is (= (ex-message e) "Failed to create individual from genome."))
      (is (= (ex-data e) {:genome "gtattccgcgtcggga"
                          :opts {}}))
      (is (instance? #?(:clj ClassCastException :cljs js/Error)
                     (ex-cause e))))))

