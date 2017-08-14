(ns trivial-partial.utils-test
  (:require [trivial-partial.utils :as sut]
            [clojure.test :as t :refer [deftest is]]))

(deftest deep-count-items-test
  (is (== 5
          (sut/deep-count-items :x '[a b c :x e
                                     {:x 10 :y 20}
                                     #{:a :b :x}
                                     :x
                                     (:p {10 :x} :q)])))
  (is (== 1
          (sut/deep-count-items :x '[{:a 10 :b 20}
                                     {:key1 :x}
                                     (1 {:key :x} :x)] :item-key :key)))
  (is (== 3
          (sut/deep-count-items 100 '[1 99 3 [102 5 {6 101} 10]]
                                :item-key #(when (number? %) (+ 10 %))
                                :compare-fn #(and (number? %2) (> %2 %1))))))

(deftest deep-find-test
  (is (= :x
         (sut/deep-find :x {:a 10 :b [20 #{1 2 :x}]})))
  (is (= nil
         (sut/deep-find :x {:a 10 :b [20 #{1 2}]})))
  (is (= {:a 10}
         (sut/deep-find 10 {:x [1 2 {:a 20} 3 #{4 5 {:a 10}}]}
                        :item-key :a)))
  (is (= {:a 100}
         (sut/deep-find 50 [1 2 3 [{:a 50} {:a 100}]]
                        :item-key :a
                        :compare-fn #(and (number? %2) (> %2 %1))))))

(deftest deep-replace-with-seq-test
  (is (= [1 2 [3 [:a :b] 4 '(10 [30 40] 20) {:x 100} #{:a :s :d}]]
         (sut/deep-replace-with-seq
          :rpl
          [2 :a 30 20 :x :s]
          [1 :rpl [3 [:rpl :b] 4 '(10 [:rpl 40] :rpl) {:rpl 100} #{:a :rpl :d}]])))

  (is (= [1 2 [3 [:a :b] 4 '(10 [30 40] 20)]]
         (sut/deep-replace-with-seq
          :rpl
          [2 :a 30 20 :x :s :a :b :c :d :e :f :g]
          [1 :rpl [3 [:rpl :b] 4 '(10 [:rpl 40] :rpl)]])))

  (is (thrown? IllegalArgumentException
               (sut/deep-replace-with-seq
                :rpl
                [2]
                [1 :rpl [3 [:rpl :b] 4 '(10 [:rpl 40] :rpl)]])))

  (is (= [1 [2 3 4] [5 [6 7] 8]]
         (sut/deep-replace-with-seq 0 [3 6]
                                    [1 [2 100 4] [5 [100 7] 8]]
                                    :item-key #(and (number? %) (- % 100)))))

  (is (= [1 [2 3 4] [5 [6 100] 8]]
         (sut/deep-replace-with-seq 50 [3 6]
                                    [1 [2 101 4] [5 [102 100] 8]]
                                    :item-key #(and (number? %) (/ % 2))
                                    :compare-fn #(and (number? %2) (> %2 %1))))))
