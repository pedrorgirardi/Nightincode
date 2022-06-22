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

(defn canonical-var-data
  "Var data defined to be persisted in the database."
  [definition]

  ;; Note:
  ;; Name row & col encode the location of the symbol.
  ;; Row & col, without name, encode the form location.

  (merge #:var {:namespace (:ns definition)
                :name (:name definition)
                :lang (:lang definition :clj)
                :row (:row definition)
                :row-end (:end-row definition)
                :col (:col definition)
                :col-end (:end-col definition)
                :name-row (:name-row definition)
                :name-row-end (:name-end-row definition)
                :name-col (:name-col definition)
                :name-col-end (:name-end-col definition)
                :defined-by (:defined-by definition)
                :filename (:filename definition)}
    (when-let [doc (:doc definition)]
      {:var/doc doc})))

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
            (cond
              (= semantic :var-definitions)
              (into tx-data (map canonical-var-data) items)

              :else
              tx-data))
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
    :row 3}


  (def conn (d/create-conn schema))

  (d/transact! conn prepared)

  (d/q '[:find  [(pull ?v [*]) ...]
         :where
         [?v :var/name schema]]
    (d/db conn))

  (d/q '[:find  [(pull ?v [:var/name]) ...]
         :where
         [?v :var/namespace nightincode.analyzer]]
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
