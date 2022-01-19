(ns erp12.ga-clj.utils
  "Utility functions. Should not be directly about evolution or GAs."
  #?(:cljs (:require-macros [erp12.ga-clj.utils :refer [with-error-context]])))

(defn min-by-cmp
  "Return the minimal element of `coll` according to the comparator."
  [cmp coll]
  (loop [remaining coll
         mn nil]
    (let [el (first remaining)]
      (if (empty? remaining)
        mn
        (recur (rest remaining)
               (if (nil? mn)
                 el
                 (if (neg? (cmp el mn)) el mn)))))))

(defn random-distinct-by
  [f coll]
  (->> coll
       shuffle
       (reduce (fn [acc el] (update acc (f el) #(or % el))) {})
       vals))

(defn- platform
  "Given a macro's environment, returns a keyword denoting the platform which the post-expansion
  code will be running in. For example, `:clj` for Clojure and `:cljs` for Clojurescript.

  Inside macros, reader conditionals will not properly identify the platform in all scenarios.
  In CLJS, the macro-expansion stage happens using Clojure (not Clojurescript) before any JS
  compilation, and thus the `:clj` reader branch will always be followed inside macros.

  See https://stackoverflow.com/a/28759406/4297413
  See https://groups.google.com/g/clojurescript/c/iBY5HaQda4A/m/5sAsgcp9ZtkJ
  "
  [env]
  (if (:ns env) ;; :ns only exists in CLJS.
    :cljs
    :clj))

(defmacro with-error-context
  "Catches errors raised when evaluating `form` and raises an `ExceptionInfo`
   containing additional data about the `context` which the exception was raised.

   If `context` contains the key `:msg`, it will be used as the message of the
   outer `ExceptionInfo`. Otherwise, a generic default is used.
   "
  [context form]
  (let [msg (or (:msg context) "Exception raised within context.")
        err-type (if (= :cljs (platform &env))
                   'js/Error
                   'java.lang.Exception)]
    `(try
       ~form
       (catch ~err-type e#
         (throw (ex-info ~msg ~(dissoc context :msg) e#))))))
