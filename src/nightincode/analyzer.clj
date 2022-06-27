(ns nightincode.analyzer
  (:require
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [clojure.tools.logging :as log]
   [clojure.pprint :as pprint]

   [datascript.core :as d]
   [clj-kondo.core :as clj-kondo]

   [nightincode.specs]))

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

(defn cursor-loc
  "Returns a loc in `locs` for `position`."
  [cursor-semthetic position]
  (reduce
   (fn [_ {:loc/keys [row col col-end] :as loc}]
     (when (and (= row (inc (:line position)))
                (<= col (inc (:character position)) col-end))
       (reduced loc)))
   nil
   (:semthetic/locs cursor-semthetic)))

(defn loc-range
  "Returns a LSP Range.

   A range in a text document expressed as (zero-based) start and end positions. 
   A range is comparable to a selection in an editor. Therefore the end position is exclusive.

   https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#range"
  [{:loc/keys [row col col-end]}]
  {:start
   {:line (dec row)
    :character (dec col)}
   :end
   {:line (dec row)
    :character (dec col-end)}})

(defn loc-location
  "Returns a LSP Location.

  https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#location"
  [filename loc]
  {:uri (->  (io/file filename) .toPath .toUri .toString)
   :range (loc-range loc)})

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

(defn namespace-semthetic
  [m]
  (merge (semthetic "namespace" m)
    {:semthetic/semantic :namespace
     :semthetic/modifier :def
     :semthetic/identifier (:name m)
     :semthetic/filename (:filename m)
     :semthetic/locs
     [{:loc/row (:name-row m)
       :loc/col  (:name-col m)
       :loc/col-end  (:name-end-col m)}]}
    (when-let [doc (:doc m)]
       {:semthetic/doc doc})))

(defn namespace-usage-semthetic
  [m]
  (merge (semthetic "namespace-usage" m)
    {:semthetic/semantic :namespace
     :semthetic/modifier :usage
     :semthetic/identifier (:to m)
     :semthetic/filename (:filename m)
     :semthetic/locs
     (cond-> [{:loc/row (:name-row m)
               :loc/col  (:name-col m)
               :loc/col-end  (:name-end-col m)}]
       ;; Include alias location (optional).
       (:alias m)
       (conj {:loc/row (:alias-row m)
              :loc/col  (:alias-col m)
              :loc/col-end  (:alias-end-col m)}))}))

(defn var-semthetic
  "Var data defined to be persisted in the database.

  Name row & col encode the location of the symbol.
  Row & col, without name, encode the form location."
  [m]
  (let [sym (symbol (name (:ns m)) (name (:name m)))]
    (merge (semthetic "var" m)
      {:semthetic/semantic :var
       :semthetic/modifier :def
       :semthetic/identifier sym
       :semthetic/filename (:filename m)
       :semthetic/locs
       [{:loc/row (:name-row m)
         :loc/col  (:name-col m)
         :loc/col-end  (:name-end-col m)}]}
      (when-let [doc (:doc m)]
       {:semthetic/doc doc}))))

(defn var-usage-semthetic
  [m]
  (let [sym (when-let [to (:to m)] (symbol (name to) (name (:name m))))]
    (merge (semthetic "var-usage" m)
      {:semthetic/semantic :var
       :semthetic/modifier :usage
       :semthetic/identifier sym
       :semthetic/filename (:filename m)
       :semthetic/locs
       [{:loc/row (or (:name-row m) (:row m))
         :loc/col  (or (:name-col m) (:col m))
         :loc/col-end  (or (:name-end-col m) (:end-col m))}]})))

(defn local-semthetic
  [m]
  (merge (semthetic "local" m)
    {:semthetic/semantic :local
     :semthetic/modifier :def
     :semthetic/identifier (:id m)
     :semthetic/filename (:filename m)
     :semthetic/locs
     [{:loc/row (:row m)
       :loc/col  (:col m)
       :loc/col-end (:end-col m)}]}))

(defn local-usage-semthetic
  [m]
  (merge (semthetic "local-usage" m)
    {:semthetic/semantic :local
     :semthetic/modifier :usage
     :semthetic/identifier (:id m)
     :semthetic/filename (:filename m)
     :semthetic/locs
     [{:loc/row (or (:name-row m) (:row m))
       :loc/col  (or (:name-col m) (:col m))
       :loc/col-end  (or (:name-end-col m) (:end-col m))}]}))

(defn keyword-semthetic
  [m]
  (merge (semthetic "keyword" m)
    {:semthetic/semantic :keyword
     :semthetic/modifier :usage
     :semthetic/filename (:filename m)
     :semthetic/identifier
     (if-let [ns (:ns m)]
       (keyword (name ns) (:name m))
       (keyword (:name m)))
     :semthetic/locs
     [{:loc/row (:row m)
       :loc/col  (:col m)
       :loc/col-end (:end-col m)}]}))

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

(defn prepare-semthetics
  "Returns tx-data for vars, var-usages, etc.., derived from `index`."
  [index]
  (into []
    (map
      (fn [[filename analysis]]
        (let [semthetics (reduce-kv
                           (fn [tx-data semantic items]
                             (let [xform (cond
                                           (= semantic :namespace-definitions)
                                           (map namespace-semthetic)

                                           (= semantic :namespace-usages)
                                           (map namespace-usage-semthetic)

                                           (= semantic :var-definitions)
                                           (map var-semthetic)

                                           (= semantic :var-usages)
                                           (map var-usage-semthetic)

                                           (= semantic :locals)
                                           (map local-semthetic)

                                           (= semantic :local-usages)
                                           (map local-usage-semthetic)

                                           (= semantic :keywords)
                                           (map keyword-semthetic))]
                               (if xform
                                 (into tx-data
                                   (comp
                                     xform
                                     (filter
                                       (fn [semthetic]
                                         (or (s/valid? :semthetic/semthetic semthetic)
                                           (log/warn
                                             (str "Invalid Semthetic:"
                                               "\n"
                                               (with-out-str (pprint/pprint semthetic))
                                               "\nExplain:\n"
                                               (s/explain-str :semthetic/semthetic semthetic)))))))
                                   items)
                                 tx-data)))
                           []
                           analysis)]
          {:file/path filename
           :file/semthetics semthetics})))
    index))

(defn ?cursor-semthetic
  "Returns a **Semthetic** under the cursor, or `nil` if there is none."
  [db {:keys [filename row col col-end]}]
  (d/q '[:find  (pull ?e [*]) .
         :in $ ?filename ?row ?col ?col-end
         :where
         [?e :semthetic/locs ?locs]
         [?e :semthetic/filename ?filename]
         [?locs :loc/row ?row]
         [?locs :loc/col ?col_]
         [?locs :loc/col-end ?col-end_]
         [(>= ?col ?col_)]
         [(<= ?col-end ?col-end_)]]
       db filename row col col-end))

(defn ?semantic-definitions [db semthetic]
  (d/q '[:find [(pull ?e [*]) ...]
         :in $ ?semantic ?identifier
         :where
         [?e :semthetic/semantic ?semantic]
         [?e :semthetic/modifier :def]
         [?e :semthetic/identifier ?identifier]]
    db (:semthetic/semantic semthetic) (:semthetic/identifier semthetic)))

(defn ?semantic-file-definitions [db semthetic]
  (d/q '[:find [(pull ?e [*]) ...]
         :in $ ?filename ?semantic ?identifier
         :where
         [?e :semthetic/filename ?filename]
         [?e :semthetic/semantic ?semantic]
         [?e :semthetic/modifier :def]
         [?e :semthetic/identifier ?identifier]]
    db (:semthetic/filename semthetic) (:semthetic/semantic semthetic) (:semthetic/identifier semthetic)))

(defn ?semantic-usages [db semthetic]
  (d/q '[:find [(pull ?e [*]) ...]
         :in $ ?semantic ?identifier
         :where
         [?e :semthetic/semantic ?semantic]
         [?e :semthetic/modifier :usage]
         [?e :semthetic/identifier ?identifier]]
    db (:semthetic/semantic semthetic) (:semthetic/identifier semthetic)))

(defn ?semantic-file-usages [db semthetic]
  (d/q '[:find [(pull ?e [*]) ...]
         :in $ ?filename ?semantic ?identifier
         :where
         [?e :semthetic/filename ?filename]
         [?e :semthetic/semantic ?semantic]
         [?e :semthetic/modifier :usage]
         [?e :semthetic/identifier ?identifier]]
    db (:semthetic/filename semthetic) (:semthetic/semantic semthetic) (:semthetic/identifier semthetic)))


(comment

  (def example1-path
    (.getPath (io/resource "example1.cljc")))

  (def nightincode-path
    "/Users/pedro/Developer/Nightincode/src")

  (def result
    (clj-kondo/run!
      {:lint [example1-path]
       :config default-clj-kondo-config}))

  (keys result)

  (:findings result)

  (keys (:analysis result))

  (def indexed
    (index (:analysis result)))

  (def prepared
    (prepare-semthetics indexed))

  (keys indexed)

  (indexed "/Users/pedro/Developer/Nightincode/src/nightincode/server.clj")


  )
