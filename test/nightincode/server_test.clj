(ns nightincode.server-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.java.io :as io]
   [clojure.tools.deps :as deps]
   [clojure.test :refer [deftest testing is]]

   [clj-kondo.core :as clj-kondo]

   [nightincode.specs]
   [nightincode.server :as server]
   [nightincode.analyzer :as analyzer]))

(deftest deps-paths-test
  (is (= ["src" "resources" "dev" "test"]
        (server/deps-paths
          (deps/slurp-deps
            (io/file (io/resource "deps_test.edn"))))))

  (is (= nil
        (server/deps-paths nil)))

  (is (= nil
        (server/deps-paths
          (deps/slurp-deps
            (io/file "foo.edn"))))))

(deftest uri->diagnostics-test
  (is (= {} (server/uri->diagnostics nil)))
  (is (= {} (server/uri->diagnostics [])))

  (testing "example1 findings"
    (let [{:keys [findings]} (clj-kondo/run!
                               {:lint [(.getPath (io/resource "example1.cljc"))]
                                :config analyzer/default-clj-kondo-config})]
      (is (= true
            (s/valid? :nightincode/diagnostics (server/uri->diagnostics findings)))))))
