(ns nightincode.specs
  (:require
   [clojure.spec.alpha :as s]))

;; -- LSP

;; -- TraceValue
;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#traceValue
(s/def :lsp/TraceValue
  #{"off"
    "messages"
    "verbose"})


;; -- InitializeParams
;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initializeParams

(s/def :lsp.InitializeParams/processId (s/nilable int?))
(s/def :lsp.InitializeParams/rootPath (s/nilable string?))
(s/def :lsp.InitializeParams/rootUri string?)
(s/def :lsp.InitializeParams/locale (s/nilable string?))
(s/def :lsp.InitializeParams/trace (s/nilable :lsp/TraceValue))
(s/def :lsp.InitializeParams/clientInfo (s/nilable map?))
(s/def :lsp.InitializeParams/capabilities (s/nilable map?))
(s/def :lsp/InitializeParams
  (s/keys
    :req-un [:lsp.InitializeParams/processId
             :lsp.InitializeParams/rootUri
             :lsp.InitializeParams/capabilities]

    :opt-un [:lsp.InitializeParams/clientInfo
             :lsp.InitializeParams/locale
             :lsp.InitializeParams/rootPath
             :lsp.InitializeParams/trace]))

;; -- InitializedParams
;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialized

(s/def :lsp/InitializedParams any?)


;; -- Position
;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#position

(s/def :lsp.Position/line nat-int?)
(s/def :lsp.Position/character nat-int?)
(s/def :lsp/Position
  (s/keys
    :req-un [:lsp.Position/line
             :lsp.Position/character]))


;; -- Range
;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#range

(s/def :lsp.Range/start :lsp/Position)
(s/def :lsp.Range/end :lsp/Position)
(s/def :lsp/Range
  (s/keys
    :req-un [:lsp.Range/start
             :lsp.Range/end]))


;; -- Diagnostic
;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic

(s/def :lsp.Diagnostic/range :lsp/Range)
(s/def :lsp.Diagnostic/code (s/or :ineteger int? :string string?))
(s/def :lsp.Diagnostic/source string?)
(s/def :lsp.Diagnostic/message string?)
(s/def :lsp.Diagnostic/severity
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
    :req-un [:lsp.Diagnostic/message
             :lsp.Diagnostic/range]
    :opt-un [:lsp.Diagnostic/code
             :lsp.Diagnostic/source
             :lsp.Diagnostic/severity]))


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



(s/def :nightincode.document-index/text string?)

(s/def :nightincode.document-index/version pos-int?)

(s/def :nightincode/document-index
  (s/map-of string? (s/keys
                      :req-un [:nightincode.document-index/text]
                      :opt-un [:nightincode.document-index/version])))

(s/def :nightincode/diagnostics
  (s/map-of string? (s/coll-of :lsp/Diagnostic)))

(s/def :nightincode/analyzer map?)


;; -- State

(s/def :nightincode/state
  (s/keys
    :opt [:lsp/InitializeParams
          :lsp/InitializedParams
          :nightincode/document-index
          :nightincode/diagnostics
          :nightincode/analyzer]))

(s/def :nightincode.state/initialized
  (s/keys
    :req [:lsp/InitializeParams
          :nightincode/analyzer]
    :opt [:nightincode/diagnostics]))
