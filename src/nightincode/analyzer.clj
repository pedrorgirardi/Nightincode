(ns nightincode.analyzer
  (:require
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
    (fn [index [semantic codeqs]]
      (merge-with merge-index index (into {}
                                      (map
                                        (fn [[filename codeqs]]
                                          {filename {semantic codeqs}}))
                                      (group-by :filename codeqs))))
    {}
    analysis))

(comment

  (def result
    (clj-kondo/run!
      {:lint ["/Users/pedro/Developer/Nightincode/test/example1.clj"]
       :config default-clj-kondo-config}))

  (keys result)

  (keys (:analysis result))


  (def index
    (reduce
      (fn [index [semantic codeqs]]
        (merge-with merge-index index (into {}
                                        (map
                                          (fn [[filename codeqs]]
                                            {filename {semantic codeqs}}))
                                        (group-by :filename codeqs))))
      {}
      (:analysis result)))

  (keys index)

  (index "/Users/pedro/Developer/Nightincode/src/nightincode/analyzer.clj")

  (clj-kondo/run!
    {:lint ["/Users/pedro/Developer/Nightincode/src/nightincode/server.clj"]
     :config default-clj-kondo-config})

  )
