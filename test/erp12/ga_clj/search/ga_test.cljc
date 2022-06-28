(ns erp12.ga-clj.search.ga-test
  (:require [clojure.test :refer [deftest is testing]]
            ;; ns must be required, even if alias isn't used, because it binds the vars.
            [erp12.ga-clj.search.ga :as g]))

(deftest make-generation-test
  (let [make-generation #'erp12.ga-clj.search.ga/make-generation]
    (is (= {:cases      [1 3 5]
            :population '({:genome "gtattc"}
                          {:genome "gtattc"}
                          {:genome "gtattc"})
            :epsilon    [0.1 0.2 0.3]
            :step       1}
           (make-generation {:pre-generation     (constantly {:cases [1 3 5]})
                             :population-factory (constantly (repeat 3 {:genome "gtattc"}))
                             :post-generation    (constantly {:epsilon [0.1 0.2 0.3]})
                             :context            {:step 1}})))))

(deftest initial-generation-test
  (let [next-generation #'erp12.ga-clj.search.ga/initial-generation]
    (is (= {:population '({:genome "gtattc" :error 100}
                          {:genome "gtattc" :error 100}
                          {:genome "gtattc" :error 100})
            :step       1}
           (next-generation {:genome-factory     (constantly "gtattc")
                             :individual-factory (fn [gn context] {:error 100})
                             :population-size    3
                             :population-factory (constantly (repeat 3 {:genome "gtattc"}))
                             :context            {:step 1}})))
    (testing "with pre- and post- generation"
      (is (= {:cases      [1 3 5]
              :population '({:genome "gtattc" :error 0}
                            {:genome "gtattc" :error 0}
                            {:genome "gtattc" :error 0})
              :epsilon    [0.1 0.2 0.3]
              :step       1}
             (next-generation {:genome-factory     (constantly "gtattc")
                               :individual-factory (fn [gn {:keys [cases]}]
                                                     (is (some? gn))
                                                     (is (some? cases))
                                                     {:error 0})
                               :population-size    3
                               :pre-generation     (fn [{:keys [step]}]
                                                     (is (= step 1))
                                                     {:cases [1 3 5]})
                               :population-factory (constantly (repeat 3 {:genome "gtattc"}))
                               :post-generation    (fn [{:keys [population cases step]}]
                                                     (is (= population (repeat 3 {:genome "gtattc" :error 0})))
                                                     (is (= cases [1 3 5]))
                                                     (is (= step 1))
                                                     {:epsilon [0.1 0.2 0.3]})
                               :context            {:step 1}}))))))


(deftest next-generation-test
  (let [next-generation #'erp12.ga-clj.search.ga/next-generation]
    (is (= {:cases      [1 3 5]
            :population '({:genome "gtattc" :error 0}
                          {:genome "gtattc" :error 0}
                          {:genome "gtattc" :error 0})
            :epsilon    [0.1 0.2 0.3]
            :step       1}
           (next-generation {:genomes            (repeat 3 "gtattc")
                             :individual-factory (fn [gn {:keys [cases]}]
                                                   (is (= gn "gtattc"))
                                                   (is (= cases [1 3 5]))
                                                   {:error 0})
                             :pre-generation     (fn [{:keys [step]}]
                                                   (is (= step 1))
                                                   {:cases [1 3 5]})
                             :population-factory (constantly (repeat 3 {:genome "gtattc"}))
                             :post-generation    (fn [{:keys [population cases step]}]
                                                   (is (= population (repeat 3 {:genome "gtattc" :error 0})))
                                                   (is (= cases [1 3 5]))
                                                   (is (= step 1))
                                                   {:epsilon [0.1 0.2 0.3]})
                             :context            {:step 1}})))))
