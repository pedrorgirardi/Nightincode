(ns build
  "Build Uberjar: clj -T:build uber"
  (:require [clojure.tools.build.api :as b]))

(def class-dir "target/classes")

(def basis (b/create-basis {:project "deps.edn"}))

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber [_]
  (clean nil)

  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})

  (b/compile-clj {:basis basis
                  :src-dirs ["src"]
                  :class-dir class-dir})

  (b/uber {:class-dir class-dir
           :uber-file "nightincode.jar"
           :basis basis
           :main 'nightincode.server}))
