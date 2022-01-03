(ns erp12.ga-clj.toolbox-test
  (:require [clojure.test :refer [deftest is testing]]
            [kixi.stats.math :as math]
            [erp12.ga-clj.toolbox :as tb]))

(deftest mean-test
  (is (= 2.0 (tb/mean [3 2 1]))))

(deftest mea-test
  (is (= 1.0 (tb/mae [5 5 5] [4 6 4]))))

(deftest mse-test
  (is (= 7.0 (tb/mse [5 5 5] [4 7 9]))))

(deftest rmse-test
  (is (= (math/sqrt 7)
         (tb/rmse [5 5 5] [4 7 9]))))

(deftest hamming-distance-test
  (is (= 2 (tb/hamming-distance "000" "101")))
  (is (= 3 (tb/hamming-distance [:a :b :c] [:x :y :z])))
  (is (= 0 (tb/hamming-distance [:x :y :z] [:x :y :z]))))

(deftest stdev-test
  (is (= (math/sqrt 7.5)
         (tb/stdev [1 2 4 5 8]))))

(deftest median-test
  (is (= 2 (tb/median [2 1 3])))
  (is (= 2.5 (tb/median [1 2 3 4]))))

(deftest mad-test
  (is (= 2.5 (tb/mad (range 10))))
  (is (= 0 (tb/mad (repeat 5 5)))))

(deftest compute-epsilon-per-case-test
  (let [error-vectors [[1 2 3]
                       [3 4 5]
                       [6 7 8]
                       [9 1 0]]]
    (is (= [1 1 0]
           (tb/compute-epsilon-per-case error-vectors {:errors-fn identity :agg-by #(apply min %)})))
    (is (= [2.5 1.5 2.5]
           (tb/compute-epsilon-per-case error-vectors {:errors-fn identity})))))

(deftest lexicase-selection-test
  (let [population [{:name :A :errors [10 5 5 15 10]}
                    {:name :B :errors [8 7 8 8 7]}
                    {:name :C :errors [73 60 0 0 1]}
                    {:name :D :errors [15 12 14 15 1]}
                    {:name :E :errors [15 12 0 106 1]}]]
    (testing "Standard lexicase selection"
      (is (= :D (:name (tb/lexicase-selection {:candidates population :cases '(4 1 0 3 2) :errors-fn :errors}))))
      (is (= :E (:name (tb/lexicase-selection {:candidates population :cases '(4 1 0 2 3) :errors-fn :errors}))))
      (is (= :B (:name (tb/lexicase-selection {:candidates population :cases '(0 3 2 4 1) :errors-fn :errors}))))
      (is (= :A (:name (tb/lexicase-selection {:candidates population :cases '(1 0 3 2 4) :errors-fn :errors}))))
      (is (= :C (:name (tb/lexicase-selection {:candidates population :cases '(2 4 3 0 1) :errors-fn :errors}))))
      (is (= :E (:name (tb/lexicase-selection {:candidates population :cases '(2 0 3 4 1) :errors-fn :errors})))))))
