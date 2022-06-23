(ns nightincode.analyzer
  (:require
   [clojure.java.io :as io]

   [datascript.core :as d]
   [clj-kondo.core :as clj-kondo]))

(def schema
  {:file/path
   {:db/unique :db.unique/identity}

   :file/semthetics
   {:db/valueType   :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/isComponent true}

   ;; Encode an ident locations.
   ;; It's a cardinality many since an entity
   ;; might encode its location in more than a single range.
   ;;
   ;; E.g. namespace-usage: its location is encoded in the range of the namespace symbol,
   ;; as well as in the range of the alias symbol.
   :semthetic/locs
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

(defn semthetic [ns m]
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
  (semthetic "namespace" m))

(defn namespace-usage-data
  [m]
  (semthetic "namespace-usage" m))

(defn var-data
  "Var data defined to be persisted in the database."
  [m]

  ;; Note:
  ;; Name row & col encode the location of the symbol.
  ;; Row & col, without name, encode the form location.

  (merge (semthetic "var" m)
    {:semthetic/ns (:ns m)
     :semthetic/locs
     [{:loc/row (:name-row m)
       :loc/col  (:name-col m)
       :loc/col-end  (:name-end-col m)}]}))

(defn var-usage-data
  [m]
  (merge (semthetic "var-usage" m)
    {:semthetic/locs
     [{:loc/row (or (:name-row m) (:row m))
       :loc/col  (or (:name-col m) (:col m))
       :loc/col-end  (or (:name-end-col m) (:end-col m))}]}))

(defn local-data
  [m]
  (merge (semthetic "local" m)
    {:semthetic/locs
     [{:loc/row (:row m)
       :loc/col  (:col m)
       :loc/col-end (:end-col m)}]}))

(defn local-usage-data
  [m]
  (semthetic "local-usage" m))

(defn keyword-data
  [m]
  (semthetic "keyword" m))

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
    (map
      (fn [[filename analysis]]
        (let [forms (reduce-kv
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

                                      (= semantic :local-usages)
                                      (map local-usage-data)

                                      (= semantic :keywords)
                                      (map keyword-data))]
                          (if xform
                            (into tx-data xform items)
                            tx-data)))
                      []
                      analysis)]
          {:file/path filename
           :file/semthetics forms})))
    index))

(defn ?vars [db {:keys [ns name]}]
  (d/q '[:find [(pull ?v [*]) ...]
         :in $ ?ns ?name
         :where
         [?v :var/ns ?ns]
         [?v :var/name ?name]]
    db ns name))

(defn ?var-usages [db {:keys [ns name]}]
  (d/q '[:find [(pull ?v [*]) ...]
         :in $ ?ns ?name
         :where
         [?v :var-usage/to ?ns]
         [?v :var-usage/name ?name]]
    db ns name))

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
  (semthetic "var"
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

  (d/transact! conn
    [{:file/path "a.clj"
      :file/semthetics
      [{:var/namespace 'person
        :var/name 'first-name}]}])

  (d/transact! conn
    [{:file/path "b.clj"
      :file/semthetics
      [{:var/namespace 'person
        :var/name 'last-name}]}])

  (d/touch (d/entity (d/db conn) [:file/path "a.clj"]))

  (d/transact! conn
    [[:db/retractEntity [:file/path "a.clj"]]])

  (d/transact! conn
    [[:db/retract [:file/path "a.clj"] :file/semthetics]])

  (d/transact! conn
    [[:db/retract [:file/path "a.clj"] :file/semthetics]

     {:file/path "a.clj"
      :file/semthetics
      [{:var/namespace 'person
        :var/name 'firstname}]}])


  (d/q '[:find  (pull ?v [*])
         :where
         [?v :var/namespace person]]
    (d/db conn))



  )
