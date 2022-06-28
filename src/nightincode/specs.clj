(ns nightincode.specs
  (:require
   [clojure.spec.alpha :as s]))

;; -- clj-kondo Finding

(s/def :clj-kondo.finding/row pos-int?)
(s/def :clj-kondo.finding/end-row pos-int?)
(s/def :clj-kondo.finding/col pos-int?)
(s/def :clj-kondo.finding/end-col pos-int?)
(s/def :clj-kondo.finding/message string?)
(s/def :clj-kondo.finding/level #{:warning :error})

(s/def :clj-kondo/finding
  (s/keys
    :req-un
    [:clj-kondo.finding/row
     :clj-kondo.finding/col
     :clj-kondo.finding/message
     :clj-kondo.finding/level]

    :opt-un
    [:clj-kondo.finding/end-row
     :clj-kondo.finding/end-col]))


;; -- Loc

(s/def :loc/row nat-int?)
(s/def :loc/col nat-int?)
(s/def :loc/col-end nat-int?)

(s/def :loc/loc
  (s/keys :req [:loc/row
                :loc/col
                :loc/col-end]))


;; -- Semthetic

(s/def :semthetic/semantic
  #{:namespace :var :local :keyword})

(s/def :semthetic/modifier
  #{:def :usage})

(s/def :semthetic/identifier
  (s/or
    :symbol symbol?
    :keyword keyword?
    :number number?
    :string string?))

(s/def :semthetic/filename string?)

(s/def :semthetic/locs
  (s/coll-of :loc/loc))

(s/def :semthetic/semthetic
  (s/keys
    :req [:semthetic/semantic
          :semthetic/modifier
          :semthetic/locs
          :semthetic/filename]
    :opt [:semthetic/identifier]))


;; -- File

(s/def :file/path string?)

(s/def :file/semthetics (s/coll-of :semthetic/semthetic))


(s/def :LSP/InitializeParams (s/map-of string? any?))

(s/def :LSP/InitializedParams (s/map-of string? any?))

(s/def :nightincode/document-index (s/map-of string? string?))

(s/def :server/state
  (s/keys
    :opt [:LSP/InitializeParams
          :LSP/InitializedParams
          :nightincode/document-index]))
