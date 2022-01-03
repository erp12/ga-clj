(ns build
  (:require [clojure.tools.build.api :as b]
            [org.corfield.build :as bb]))

(defn- process-result
  [{:keys [exit out err]}]
  (when out (println out))
  (when err (println err))
  (when-not (zero? exit)
    (System/exit exit)))

(defn test-clj
  [opts]
  (println "\nRunning CLJ Tests...")
  (bb/run-tests opts)
  opts)

(defn test-cljs
  [opts]
  (println "\nRunning CLJS Tests...")
  (process-result (b/process {:command-args ["clojure" "-M:test-cljs" "compile" "test"]}))
  (process-result (b/process {:command-args ["node" "out/node-tests.js"]}))
  opts)

(defn example
  [opts]
  ;; @todo Pass args to example file.
  (process-result (b/process {:command-args ["clojure" "-M:examples" "-m" (name (:ns opts))]})))

(defn examples
  [opts]
  (doseq [example-ns ['erp12.ga-clj.examples.alphabet
                      'erp12.ga-clj.examples.symbolic-regression]]
    (println "\nRunning example" example-ns)
    ;; @todo Pass smaller population sizes and max generations to examples via command args to keep CI fast.
    (example (assoc opts :ns example-ns))))

(defn ci
  [opts]
  (-> opts test-clj test-cljs examples))
