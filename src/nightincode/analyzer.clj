(ns nightincode.analyzer
  (:require
   [clojure.java.io :as io]

   [datascript.core :as d]
   [clj-kondo.core :as clj-kondo]))

(def schema
  {:document/filename
   {:db/unique :db.unique/identity}

   :document/parts
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

(defn persisted-var
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
                                          {filename
                                           {:analysis
                                            {semantic items}}}))
                                      (group-by :filename items))))
    {}
    analysis))

(comment

  (def example1-path
    (.getPath (io/resource "example1.cljc")))

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


  (d/transact! conn
    [{:document/filename "A"
      :document/parts
      [{:var/namespace 'person
        :var/name 'name}

       {:var/namespace 'person
        :var/name 'lastname}]}])

  (d/touch (d/entity (d/db conn) [:document/filename "A"]))

  (d/transact! conn
    [[:db/retractEntity [:document/filename "A"]]])

  (d/transact! conn
    [[:db/retract [:document/filename "A"] :document/parts]])

  (d/transact! conn
    [[:db/retract [:document/filename "A"] :document/parts]

     {:document/filename "A"
      :document/parts
      [{:var/namespace 'person
        :var/name 'firstname}]}])


  (d/q '[:find  (pull ?v [*])
         :where
         [?v :var/namespace person]]
    (d/db conn))



  )
