(ns nightincode.analyzer
  (:require
   [clojure.java.io :as io]

   [datascript.core :as d]
   [clj-kondo.core :as clj-kondo]))

(def schema
  {:var/namespace+name+lang
   {:db/tupleAttrs [:var/namespace :var/name :var/lang]
    :db/unique :db.unique/identity}})

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
    [{:var/namespace 'person
      :var/name 'name}])

  (d/transact! conn
    [{:var/namespace+name ['person 'name]
      :var/row 0
      :var/row-end 1
      :var/col 0
      :var/col-end 1}])

  (d/touch (d/entity (d/db conn) [:var/namespace+name ['person 'name]]))


  (d/q '[:find  (pull ?v [*])
         :where
         [?v :var/row 0]
         [?v :var/col 0]]
    (d/db conn))



  )
