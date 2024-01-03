(ns nightincode.analyzer-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest testing is]]

   [clj-kondo.core :as clj-kondo]

   [nightincode.analyzer :as analyzer]))

(deftest default-clj-kondo-config-test
  (let [{:keys [analysis]} (clj-kondo/run!
                             {:lint [(.getPath (io/resource "example1.cljc"))]
                              :config analyzer/default-clj-kondo-config})]
    (testing "Namespace example1 definition"
      (is (= '[{:lang :clj
                :row 1
                :end-row 3
                :col 1
                :end-col 15
                :name example1
                :name-col 5
                :name-end-col 13
                :name-end-row 1
                :name-row 1
                :doc "Namespace for testing."
                :meta {:foo :bar}}
               {:lang :cljs
                :row 1
                :end-row 3
                :col 1
                :end-col 15
                :name example1
                :name-col 5
                :name-end-col 13
                :name-end-row 1
                :name-row 1
                :doc "Namespace for testing."
                :meta {:foo :bar}}]
            (into []
              (map
                (fn [data]
                  (dissoc data :filename)))
              (:namespace-definitions analysis)))))))

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

(deftest prepare-semthetics-test
  (let [index {"example1.cljc"
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
                   :row 3}]}}]
    (is (= '[{:file/path "example1.cljc"
              :file/semthetics
              [{:semthetic/filename "example1.cljc"
                :semthetic/identifier example1/a
                :semthetic/locs [#:loc{:col 3 :col-end 4 :row 4}]
                :semthetic/modifier :def
                :semthetic/semantic :var
                :var/col 1
                :var/defined-by clojure.core/def
                :var/end-col 5
                :var/end-row 5
                :var/filename "example1.cljc"
                :var/lang :clj
                :var/name a
                :var/name-col 3
                :var/name-end-col 4
                :var/name-end-row 4
                :var/name-row 4
                :var/ns example1
                :var/row 3}]}]
          (analyzer/prepare-semthetics index)))))
