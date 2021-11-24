(ns erp12.ga-clj.utils-test
  (:require [clojure.test :refer [deftest is testing]]
            [erp12.ga-clj.utils :refer [min-by-cmp with-error-context]])
  #?(:clj (:import (clojure.lang ExceptionInfo))))

(deftest min-by-cmp-test
  (is (= (min-by-cmp (comparator <) [2 0 1])
         0))
  (is (nil? (min-by-cmp (comparator <) [])))
  (is (= (min-by-cmp (comparator <) [1 1 1])
         1)))

(deftest with-error-context-test
  (let [e (try
            (with-error-context {:msg "Oops!" :extra-data 10} (count 1))
            (catch #?(:clj ExceptionInfo :cljs js/Error) e
              e))]
    (is (= (ex-message e) "Oops!"))
    (is (= (ex-data e) {:extra-data 10}))
    (is (instance? #?(:clj UnsupportedOperationException :cljs js/Error)
                   (ex-cause e)))))
