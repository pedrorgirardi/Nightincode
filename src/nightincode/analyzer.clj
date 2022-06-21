(ns nightincode.analyzer
  (:require
   [clojure.java.io :as io]

   [clj-kondo.core :as clj-kondo]))

(def default-clj-kondo-config
  {:analysis
   {:arglists true
    :locals true
    :keywords true
    :java-class-usages true}

   :output
   {:canonical-paths true}})

(defn merge-index [a b]
    (merge-with into a b))

(defn index [analysis]
  (reduce
    (fn [index [semantic items]]
      (merge-with merge-index index (into {}
                                      (map
                                        (fn [[filename items]]
                                          {filename
                                           {:analysis
                                            {semantic items}}}))
                                      (group-by :filename items))))
    {}
    analysis))

(comment

  (def example1-path
    (.getPath (io/resource "example1.clj")))

  (def result
    (clj-kondo/run!
      {:lint [example1-path]
       :config default-clj-kondo-config}))

  (keys result)

  (keys (:analysis result))

  (def indexed
    (index (:analysis result)))

  (keys indexed)

  (indexed example1-path)


  )
