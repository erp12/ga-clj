(ns erp12.ga-clj.toolbox-test
  (:require [clojure.test :refer [deftest is testing]]
            [erp12.ga-clj.toolbox :refer [lexicase-selection]]))

(deftest lexicase-selection-test
  (let [population [{:name :A :errors [10 5 5 15 10]}
                    {:name :B :errors [8 7 8 8 7]}
                    {:name :C :errors [73 60 0 0 1]}
                    {:name :D :errors [15 12 14 15 1]}
                    {:name :E :errors [15 12 0 106 1]}]]
    (testing "Standard lexicase selection"
      (is (= :D (:name (lexicase-selection population '(4 1 0 3 2) :errors))))
      (is (= :E (:name (lexicase-selection population '(4 1 0 2 3) :errors))))
      (is (= :B (:name (lexicase-selection population '(0 3 2 4 1) :errors))))
      (is (= :A (:name (lexicase-selection population '(1 0 3 2 4) :errors))))
      (is (= :C (:name (lexicase-selection population '(2 4 3 0 1) :errors))))
      (is (= :E (:name (lexicase-selection population '(2 0 3 4 1) :errors)))))
    (testing "Lexicase selection running out of cases"
      (let [parent (:name (lexicase-selection (conj population {:name :F :errors [73 60 0 0 1]})
                                              '(2 4 3 1 0)
                                              :errors))]
        (is (or (= parent :C)
                (= parent :F)))))))
