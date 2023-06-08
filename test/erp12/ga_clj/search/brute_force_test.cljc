(ns erp12.ga-clj.search.brute-force-test
  (:require [clojure.test :refer [deftest is]]
            [erp12.ga-clj.search.brute-force :refer [brute-force-lazy-sequence]]))

(deftest brute-force-lazy-sequence-test
  (is (= '(() (:a) (:b) (:c) (:a :a) (:a :b))
         (take 6 (brute-force-lazy-sequence [:a :b :c])))))
