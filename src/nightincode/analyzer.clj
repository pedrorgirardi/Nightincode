(ns nightincode.analyzer
  (:require
   [clojure.java.io :as io]

   [datascript.core :as d]
   [clj-kondo.core :as clj-kondo]))

(def schema
  {:file/path
   {:db/unique :db.unique/identity}

   :file/forms
   {:db/valueType   :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/isComponent true}})

(def default-clj-kondo-config
  {:analysis
   {:arglists true
    :locals true
    :keywords true
    :java-class-usages true}

   :output
   {:canonical-paths true}})

(defn mnamespaced [ns m]
  (into {}
    (comp
      (remove
        (fn [[_ v]]
          (nil? v)))
      (map
        (fn [[k v]]
          [(keyword ns (name k)) v])))
    m))

(defn namespace-data
  [m]
  (mnamespaced "namespace" m))

(defn namespace-usage-data
  [m]
  (mnamespaced "namespace-usage" m))

(defn var-data
  "Var data defined to be persisted in the database."
  [m]

  ;; Note:
  ;; Name row & col encode the location of the symbol.
  ;; Row & col, without name, encode the form location.

  (mnamespaced "var" m))

(defn var-usage-data
  [m]
  (mnamespaced "var-usage" m))

(defn local-data
  [m]
  (mnamespaced "local" m))

(defn local-usage-data
  [m]
  (mnamespaced "local-usage" m))

(defn keyword-data
  [m]
  (mnamespaced "keyword" m))

(defn merge-index [a b]
    (merge-with into a b))

(defn index [analysis]
  (reduce
    (fn [index [semantic items]]
      (merge-with merge-index index (into {}
                                      (map
                                        (fn [[filename items]]
                                          {filename {semantic items}}))
                                      (group-by :filename items))))
    {}
    analysis))

(defn prepare
  "Returns tx-data for vars, var-usages, etc.., derived from `index`."
  [index]
  (into []
    (mapcat
      (fn [[_ analysis]]
        (reduce-kv
          (fn [tx-data semantic items]
            (let [xform (cond
                          (= semantic :namespace-definitions)
                          (map namespace-data)

                          (= semantic :namespace-usages)
                          (map namespace-usage-data)

                          (= semantic :var-definitions)
                          (map var-data)

                          (= semantic :var-usages)
                          (map var-usage-data)

                          (= semantic :locals)
                          (map local-data)

                          (= semantic :keywords)
                          (map keyword-data))]
              (if xform
                (into tx-data xform items)
                tx-data)))
          []
          analysis)))
    index))

(comment

  (def example1-path
    (.getPath (io/resource "example1.cljc")))

  (def nightincode-path
    "/Users/pedro/Developer/Nightincode/src")

  (def result
    (clj-kondo/run!
      {:lint [nightincode-path]
       :config default-clj-kondo-config}))

  (keys result)

  (keys (:analysis result))

  (def indexed
    (index (:analysis result)))

  (def prepared
    (prepare indexed))

  (keys indexed)

  (indexed "/Users/pedro/Developer/Nightincode/src/nightincode/server.clj")


  ;; Example of a Var definition:
  (mnamespaced "var"
    '{:end-row 5,
      :name-end-col 4,
      :name-end-row 4,
      :name-row 4,
      :ns example1,
      :name a,
      :defined-by clojure.core/def,
      :lang :clj,
      :filename "/Users/pedro/Developer/Nightincode/test/example1.cljc",
      :col 1,
      :name-col 3,
      :end-col 5,
      :row 3})


  (def conn (d/create-conn schema))

  (def tx-report (d/transact! conn prepared))

  ;; Every Namespace:
  (d/q '[:find  [(pull ?v [:namespace/name]) ...]
         :where
         [?v :namespace/filename]]
    (d/db conn))

  ;; Every Namespace usage:
  (d/q '[:find  [(pull ?v [:namespace-usage/to :namespace-usage/lang]) ...]
         :where
         [?v :namespace-usage/filename]]
    (d/db conn))

  ;; Every Var:
  (d/q '[:find  [(pull ?v [:var/ns :var/name]) ...]
         :where
         [?v :var/filename]]
    (d/db conn))

  ;; Every Var usage:
  (d/q '[:find  [(pull ?v [:var-usage/from :var-usage/to :var-usage/name]) ...]
         :where
         [?v :var-usage/filename]]
    (d/db conn))





  (d/transact! conn
    [{:file/path "a.clj"
      :file/forms
      [{:var/namespace 'person
        :var/name 'first-name}]}])

  (d/transact! conn
    [{:file/path "b.clj"
      :file/forms
      [{:var/namespace 'person
        :var/name 'last-name}]}])

  (d/touch (d/entity (d/db conn) [:file/path "a.clj"]))

  (d/transact! conn
    [[:db/retractEntity [:file/path "a.clj"]]])

  (d/transact! conn
    [[:db/retract [:file/path "a.clj"] :file/forms]])

  (d/transact! conn
    [[:db/retract [:file/path "a.clj"] :file/forms]

     {:file/path "a.clj"
      :file/forms
      [{:var/namespace 'person
        :var/name 'firstname}]}])


  (d/q '[:find  (pull ?v [*])
         :where
         [?v :var/namespace person]]
    (d/db conn))



  )
