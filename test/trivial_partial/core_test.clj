(ns trivial-partial.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as s]
            [clojure.walk :refer [prewalk-replace macroexpand-all]]
            [trivial-partial.utils :refer [deep-replace-with-seq]]
            [trivial-partial.core :refer :all]))

(defn- find-generated-args [data]
  (->> data
     (tree-seq coll? seq)
     (filter #(and (symbol? %)
                 (s/starts-with? (name %) "ARG_")))
     distinct))

(defn- replace-generated-args [data]
  (prewalk-replace (zipmap (find-generated-args data)
                           (map (comp keyword (partial str "ARG_"))
                                (range)))
                   data))

(defmacro generated-forms-equal? [expected actual]
  (let [actual-rep (-> actual macroexpand-all replace-generated-args)]
    `(= '~expected '~actual-rep)))

(defn- subclass? [child-class parent-class]
  ((conj (supers child-class) child-class) parent-class))

(defmacro expansion-throws? [error-class form]
  (try (macroexpand-all form)
       nil
       (catch Throwable err
         `(subclass? ~(class err) ~error-class))))

(deftest make-partial-test
  (testing "simple make-partial"
    (is (generated-forms-equal? (fn* ([:ARG_0 :ARG_1]
                                      (+ 1 2 :ARG_0 4 :ARG_1 6)))
                                (make-partial + [1 2 __ 4 __ 6]))))
  (testing "make-partial with custom placeholder"
    (is (generated-forms-equal? (fn* ([:ARG_0 :ARG_1 :ARG_2]
                                      (str 1 :ARG_0 [3 :ARG_1 5] (6 :ARG_2))))
                                (make-partial str [1 __x [3 __x 5] (6 __x)]
                                              :placeholder __x))))
  (testing "make-partial with literal sets and maps without placeholders"
    (is (generated-forms-equal? (fn* ([:ARG_0]
                                      (str 1 2 :ARG_0 {:a 10 :b #{1 2 3}})))
                                (make-partial str [1 2 __ {:a 10 :b #{1 2 3}}]))))
  (testing "make-partial with literal sets and maps with placeholders should throw error"
    (is (expansion-throws? AssertionError
                           (make-partial str [1 2 __ {:a 10 :b #{1 __ 3}}])))
    
    (is (expansion-throws? AssertionError
                           (make-partial str [1 2 __ {:a 10 :b #{1 {:x __} 3}}])))))
