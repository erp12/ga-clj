(ns erp12.ga-clj.search.ga-test
  (:require [clojure.test :refer [deftest is]]
    ;; ns must be required, even if alias isn't used, because it binds the vars.
            [erp12.ga-clj.search.ga]))

(deftest init-state-test
  (let [init-state #'erp12.ga-clj.search.ga/init-state]
    (is (= (init-state {:genome-factory  (constantly "gtattc")
                        :population-size 2
                        :mapper          map})
           {:step    0
            :genomes (list "gtattc" "gtattc")}))))

(deftest eval-generation-test
  (let [eval-generation #'erp12.ga-clj.search.ga/eval-generation]
    (is (= (eval-generation {:step    0
                             :genomes (list "gtattc" "gtattc")}
                            {:pre-eval  #(assoc % :pre true)
                             :evaluator (fn [gn _] {:error (count gn)})
                             :post-eval #(assoc % :post true)
                             :mapper    map})
           {:step        0
            :pre         true
            :individuals (list {:genome "gtattc" :error 6}
                               {:genome "gtattc" :error 6})
            :post        true}))))

(deftest next-generation-test
  (let [next-generation #'erp12.ga-clj.search.ga/next-generation]
    (is (= (next-generation {:step        0
                             :pre         true
                             :individuals (list {:genome "gtattc" :error 6}
                                                {:genome "gtattc" :error 6})
                             :post        true}
                            {:breed           (fn [state] "gtattc")
                             :population-size 2
                             :mapper          map})
           {:step    1
            :pre     true
            :genomes (list "gtattc" "gtattc")
            :post    true}))))
