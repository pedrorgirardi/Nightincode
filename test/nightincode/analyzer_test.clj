(ns nightincode.analyzer-test
  (:require
   [clojure.test :refer [deftest is]]

   [nightincode.analyzer :as analyzer]))

(deftest merge-index-test
  (is (= {:a [:x :y :z]} (analyzer/merge-index {:a [:x :y]} {:a [:z]}))))
