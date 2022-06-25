(ns nightincode.analyzer-test
  (:require
   [clojure.test :refer [deftest is]]

   [nightincode.analyzer :as analyzer]))

(deftest merge-index-test
  (is (= {:a [:x :y :z]} (analyzer/merge-index {:a [:x :y]} {:a [:z]}))))

(deftest index-test
  (let [var-definitions ['{:col 1,
                           :defined-by clojure.core/def,
                           :end-col 5,
                           :end-row 5,
                           :filename "example1.cljc",
                           :lang :clj,
                           :name a,
                           :name-col 3,
                           :name-end-col 4,
                           :name-end-row 4,
                           :name-row 4,
                           :ns example1,
                           :row 3}]]
    (is (= {"example1.cljc"
            {:var-definitions var-definitions}}
          (analyzer/index
            {:var-definitions var-definitions})))))

#_(deftest prepare-test
  (is (= '[{:var/col 1,
            :var/col-end 5,
            :var/defined-by clojure.core/def,
            :var/filename "example1.cljc",
            :var/lang :clj,
            :var/name a,
            :var/name-col 3,
            :var/name-col-end 4,
            :var/name-row 4,
            :var/name-row-end 4,
            :var/namespace example1,
            :var/row 3,
            :var/row-end 5}]
        (analyzer/prepare
          {"example1.cljc"
           {:var-definitions
            '[{:col 1,
               :defined-by clojure.core/def,
               :end-col 5,
               :end-row 5,
               :filename "example1.cljc",
               :lang :clj,
               :name a,
               :name-col 3,
               :name-end-col 4,
               :name-end-row 4,
               :name-row 4,
               :ns example1,
               :row 3}]}}))))
