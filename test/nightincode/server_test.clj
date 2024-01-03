(ns nightincode.server-test
  (:require
   [clojure.java.io :as io]
   [clojure.tools.deps :as deps]
   [clojure.test :refer [deftest is]]

   [nightincode.server :as server]))

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
  (is (= {} (server/uri->diagnostics []))))
