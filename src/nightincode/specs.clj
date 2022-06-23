(ns nightincode.specs
  (:require
   [clojure.spec.alpha :as s]))

(s/def :loc/row nat-int?)
(s/def :loc/col nat-int?)
(s/def :loc/col-end nat-int?)

(s/def :loc/loc
  (s/keys :req [:loc/row
                :loc/col
                :loc/col-end]))

(s/def :semthetic/semantic
  #{:def :usage})

(s/def :semthetic/qualifier
  #{:namespace :var :local})

(s/def :semthetic/identifier
  (s/or :symbol symbol? :number number? :string string?))

(s/def :semthetic/locs
  (s/coll-of :loc/loc))

(s/def :semthetic/semthetic
  (s/keys :opt [:semthetic/semantic
                :semthetic/qualifier
                :semthetic/identifier
                :semthetic/locs]))
