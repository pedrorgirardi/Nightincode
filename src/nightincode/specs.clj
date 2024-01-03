(ns nightincode.specs
  (:require
   [clojure.spec.alpha :as s]))

;; -- LSP

;; -- Position
;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#position

(s/def :lsp.position/line nat-int?)
(s/def :lsp.position/character nat-int?)
(s/def :lsp/Position
  (s/keys
    :req-un [:lsp.position/line
             :lsp.position/character]))


;; -- Range
;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#range

(s/def :lsp.range/start :lsp/Position)
(s/def :lsp.range/end :lsp/Position)
(s/def :lsp/Range
  (s/keys
    :req-un [:lsp.range/start
             :lsp.range/end]))


;; -- Diagnostic
;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic

(s/def :lsp.diagnostic/range :lsp/Range)
(s/def :lsp.diagnostic/code (s/or :ineteger int? :string string?))
(s/def :lsp.diagnostic/source string?)
(s/def :lsp.diagnostic/message string?)

;; See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnosticSeverity
(s/def :lsp.diagnostic/severity
  #{;; Error
    1
    ;; Warning
    2
    ;; Information
    3
    ;; Hint
    4})

(s/def :lsp/Diagnostic
  (s/keys
    :req-un [:lsp.diagnostic/message
             :lsp.diagnostic/range]
    :opt-un [:lsp.diagnostic/code
             :lsp.diagnostic/source
             :lsp.diagnostic/severity]))


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

(s/def :nightincode.document-index/text string?)

(s/def :nightincode.document-index/version pos-int?)

(s/def :nightincode/document-index
  (s/map-of string? (s/keys
                      :req-un [:nightincode.document-index/text]
                      :opt-un [:nightincode.document-index/version])))

(s/def :nightincode/diagnostics
  (s/map-of string? (s/coll-of :lsp/Diagnostic)))

(s/def :server/state
  (s/keys
    :opt [:LSP/InitializeParams
          :LSP/InitializedParams
          :nightincode/document-index
          :nightincode/diagnostics]))


(comment

  (require '[exoscale.lingo :as lingo])

  (lingo/explain :lsp.position/line -1)

  )
