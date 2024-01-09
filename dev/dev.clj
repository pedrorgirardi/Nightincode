(ns dev
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.tools.deps :as deps]

   [datascript.core :as d]

   [nightincode.server :as server]
   [nightincode.analyzer :as analyzer]))

(comment

  (server/window-showMessage (server/_out @server/state-ref)
    {:type server/MessageType_Info
     :message "Hello!"})


  (def root-path (server/_root-path @server/state-ref))


  (deps/slurp-deps (io/file root-path "foo.edn"))
  ;; => nil


  (def deps-file
    (io/file root-path "deps.edn"))

  (server/deps-paths (deps/slurp-deps deps-file))
  ;; =>
  ["src" "resources" "dev" "test"]

  (def basis
    (deps/create-basis
      {:projet deps-file}))

  (:argmap basis)
  (:classpath-roots basis)

  (deps/calc-basis basis)

  (deps/print-tree (:libs basis))


  ;; ----------------------------------------------


  (.getScheme (java.net.URI. "untitled:Untitled-1"))
  ;; =>
  "untitled"

  (.getScheme (java.net.URI. "file:///Users/pedro/Developer/Nightincode/src/nightincode/server.clj"))
  ;; =>
  "file"

  (server/uri-path nil)
  (server/uri-path "untitled:Untitled-1")
  (server/uri-path "file:///Users/pedro/Developer/Nightincode/src/nightincode/server.clj")


  (keys @server/state-ref)

  (:nightincode/document-index @server/state-ref)


  (server/_diagnostics @server/state-ref)
  ;; =>
  {"file:///Users/pedro/Developer/Nightincode/test/example1.cljc"
   [{:range {:start {:line 11 :character 5} :end {:line 11 :character 8}}
     :source "Nightincode"
     :message "Expected: number, received: string."
     :severity 1}]}



  (:lsp/InitializeParams @server/state-ref)
  ;; =>
  {:processId 26872
   :clientInfo {:name "Visual Studio Code" :version "1.85.1"}
   :locale "en"
   :rootPath "/Users/pedro/Developer/Nightincode"
   :rootUri "file:///Users/pedro/Developer/Nightincode"
   :capabilities
   {:workspace
    {:diagnostics {:refreshSupport true}
     :workspaceFolders true
     :symbol
     {:dynamicRegistration true
      :symbolKind {:valueSet [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26]}
      :tagSupport {:valueSet [1]}
      :resolveSupport {:properties ["location.range"]}}
     :semanticTokens {:refreshSupport true}
     :didChangeWatchedFiles {:dynamicRegistration true :relativePatternSupport true}
     :inlayHint {:refreshSupport true}
     :workspaceEdit
     {:documentChanges true
      :resourceOperations ["create" "rename" "delete"]
      :failureHandling "textOnlyTransactional"
      :normalizesLineEndings true
      :changeAnnotationSupport {:groupsOnLabel true}}
     :inlineValue {:refreshSupport true}
     :foldingRange {:refreshSupport true}
     :configuration true
     :executeCommand {:dynamicRegistration true}
     :fileOperations
     {:dynamicRegistration true
      :didCreate true
      :didRename true
      :didDelete true
      :willCreate true
      :willRename true
      :willDelete true}
     :didChangeConfiguration {:dynamicRegistration true}
     :codeLens {:refreshSupport true}
     :applyEdit true}
    :textDocument
    {:definition {:dynamicRegistration true :linkSupport true}
     :typeHierarchy {:dynamicRegistration true}
     :selectionRange {:dynamicRegistration true}
     :formatting {:dynamicRegistration true}
     :diagnostic {:dynamicRegistration true :relatedDocumentSupport false}
     :documentHighlight {:dynamicRegistration true}
     :semanticTokens
     {:formats ["relative"]
      :serverCancelSupport true
      :tokenTypes
      ["namespace"
       "type"
       "class"
       "enum"
       "interface"
       "struct"
       "typeParameter"
       "parameter"
       "variable"
       "property"
       "enumMember"
       "event"
       "function"
       "method"
       "macro"
       "keyword"
       "modifier"
       "comment"
       "string"
       "number"
       "regexp"
       "operator"
       "decorator"]
      :requests {:range true :full {:delta true}}
      :augmentsSyntaxTokens true
      :tokenModifiers
      ["declaration"
       "definition"
       "readonly"
       "static"
       "deprecated"
       "abstract"
       "async"
       "modification"
       "documentation"
       "defaultLibrary"]
      :multilineTokenSupport false
      :overlappingTokenSupport false
      :dynamicRegistration true}
     :linkedEditingRange {:dynamicRegistration true}
     :rename
     {:dynamicRegistration true
      :prepareSupport true
      :prepareSupportDefaultBehavior 1
      :honorsChangeAnnotations true}
     :references {:dynamicRegistration true}
     :documentSymbol
     {:dynamicRegistration true
      :symbolKind {:valueSet [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26]}
      :hierarchicalDocumentSymbolSupport true
      :tagSupport {:valueSet [1]}
      :labelSupport true}
     :inlayHint
     {:dynamicRegistration true
      :resolveSupport
      {:properties ["tooltip" "textEdits" "label.tooltip" "label.location" "label.command"]}}
     :hover {:dynamicRegistration true :contentFormat ["markdown" "plaintext"]}
     :onTypeFormatting {:dynamicRegistration true}
     :inlineValue {:dynamicRegistration true}
     :synchronization {:dynamicRegistration true :willSave true :willSaveWaitUntil true :didSave true}
     :foldingRange
     {:dynamicRegistration true
      :rangeLimit 5000
      :lineFoldingOnly true
      :foldingRangeKind {:valueSet ["comment" "imports" "region"]}
      :foldingRange {:collapsedText false}}
     :codeAction
     {:dynamicRegistration true
      :isPreferredSupport true
      :disabledSupport true
      :dataSupport true
      :resolveSupport {:properties ["edit"]}
      :codeActionLiteralSupport
      {:codeActionKind
       {:valueSet
        [""
         "quickfix"
         "refactor"
         "refactor.extract"
         "refactor.inline"
         "refactor.rewrite"
         "source"
         "source.organizeImports"]}}
      :honorsChangeAnnotations true}
     :declaration {:dynamicRegistration true :linkSupport true}
     :publishDiagnostics
     {:relatedInformation true
      :versionSupport false
      :tagSupport {:valueSet [1 2]}
      :codeDescriptionSupport true
      :dataSupport true}
     :rangeFormatting {:dynamicRegistration true :rangesSupport true}
     :codeLens {:dynamicRegistration true}
     :documentLink {:dynamicRegistration true :tooltipSupport true}
     :typeDefinition {:dynamicRegistration true :linkSupport true}
     :signatureHelp
     {:dynamicRegistration true
      :signatureInformation
      {:documentationFormat ["markdown" "plaintext"]
       :parameterInformation {:labelOffsetSupport true}
       :activeParameterSupport true}
      :contextSupport true}
     :implementation {:dynamicRegistration true :linkSupport true}
     :completion
     {:dynamicRegistration true
      :contextSupport true
      :completionItem
      {:preselectSupport true
       :documentationFormat ["markdown" "plaintext"]
       :resolveSupport {:properties ["documentation" "detail" "additionalTextEdits"]}
       :snippetSupport true
       :commitCharactersSupport true
       :insertTextModeSupport {:valueSet [1 2]}
       :insertReplaceSupport true
       :labelDetailsSupport true
       :deprecatedSupport true
       :tagSupport {:valueSet [1]}}
      :insertTextMode 2
      :completionItemKind
      {:valueSet [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25]}
      :completionList
      {:itemDefaults ["commitCharacters" "editRange" "insertTextFormat" "insertTextMode" "data"]}}
     :colorProvider {:dynamicRegistration true}
     :callHierarchy {:dynamicRegistration true}}
    :window
    {:showMessage {:messageActionItem {:additionalPropertiesSupport true}}
     :showDocument {:support true}
     :workDoneProgress true}
    :general
    {:staleRequestSupport
     {:cancel true
      :retryOnContentModified
      ["textDocument/semanticTokens/full"
       "textDocument/semanticTokens/range"
       "textDocument/semanticTokens/full/delta"]}
     :regularExpressions {:engine "ECMAScript" :version "ES2020"}
     :markdown {:parser "marked" :version "1.1.0"}
     :positionEncodings ["utf-16"]}
    :notebookDocument {:synchronization {:dynamicRegistration true :executionSummarySupport true}}}
   :trace "messages"
   :workspaceFolders [{:uri "file:///Users/pedro/Developer/Nightincode" :name "Nightincode"}]}


  ;; ----------------------------------------------


  (def classpath-db-storage (d/file-storage (io/file root-path ".nightincode" "db")))

  (or
    (d/restore-conn classpath-db-storage)
    (d/create-conn analyzer/schema {:storage classpath-db-storage}))


  (def conn (server/_analyzer-conn-paths @server/state-ref))
  (def conn-classpath (server/_analyzer-conn-classpath @server/state-ref))


  (analyzer/?semantic-definitions (d/db conn-classpath)
    {:semthetic/semantic :var
     :semthetic/identifier 'clojure.core/map})
  

  ;; Open JAR & create temporary file:
  (let [[jar clj] (str/split "/Users/pedro/.m2/repository/org/clojure/clojure/1.11.1/clojure-1.11.1.jar:clojure/core.clj" #":")]
    (with-open [zf (java.util.zip.ZipFile. jar)]
      (reduce
        (fn [_ e]
          (when (= (.getName e) clj)
            (let [tmp (java.io.File/createTempFile (format "%s_" (.getName e)) nil)]
              (with-open [in (.getInputStream zf e)]
                (io/copy in tmp))

              (reduced tmp))))
        nil
        (enumeration-seq (.entries zf)))))


  (def tmp (io/file (System/getProperty "java.io.tmpdir") "clojure/core.clj"))

  (io/make-parents tmp)

  (when (.exists tmp)
    (io/delete-file tmp))



  ;; ----------------------------------------------


  ;; Every Namespace:
  (d/q '[:find  [(pull ?v [:semthetic/identifier]) ...]
         :where
         [?v :semthetic/semantic :namespace]]
    (d/db conn))

  ;; Every Namespace usage:
  (d/q '[:find  [(pull ?v [:semthetic/identifier]) ...]
         :where
         [?v :semthetic/semantic :namespace]
         [?v :semthetic/modifier :usage]]
    (d/db conn))

  ;; A particular namespace usage `nightincode.analyzer`:
  (d/q '[:find  [(pull ?v [*]) ...]
         :where
         [?v :semthetic/semantic :namespace]
         [?v :semthetic/modifier :usage]
         [?v :semthetic/identifier nightincode.analyzer]]
    (d/db conn))

  ;; Every Var:
  (d/q '[:find  [(pull ?v [:semthetic/identifier]) ...]
         :where
         [?v :semthetic/semantic :var]]
    (d/db conn))

  ;; Every Var usage:
  (d/q '[:find  ?identifier
         :where
         [?v :semthetic/semantic :var]
         [?v :semthetic/modifier :usage]
         [?v :semthetic/identifier ?identifier]]
    (d/db conn))

  ;; Var usage missing name row & rol:
  (d/q '[:find  [(pull ?v [*]) ...]
         :where
         [?v :semthetic/semantic :var]
         [(missing? $ ?v :var-usage/name-row)]]
    (d/db conn))

  ;; Every local:
  (d/q '[:find  [(pull ?v [*]) ...]
         :where
         [?v :semthetic/semantic :local]]
    (d/db conn))

  ;; Every local usage:
  (d/q '[:find  [(pull ?v [*]) ...]
         :where
         [?v :semthetic/semantic :local]
         [?v :semthetic/modifier :usage]]
    (d/db conn))

  ;; Local usage missing name row & rol:
  (d/q '[:find  [(pull ?v [*]) ...]
         :where
         [?v :semthetic/semantic :local]
         [?v :semthetic/modifier :usage]
         [(missing? $ ?v :local-usage/name-row)]]
    (d/db conn))


  )

(comment

  (require '[clojure.edn :as edn])

  (def s (java.net.Socket. "localhost" 5555))

  (.close s)

  ;; ---

  (def r
    (java.io.BufferedReader.
      (java.io.InputStreamReader.
        (.getInputStream s))))

  (.close r)

  ;; ---

  (def w
    (java.io.PrintWriter.
      (.getOutputStream s) true))

  (.close w)

  ;; ---

  (.println w "(do (println 1) *ns*)")

  ;; pREPL
  ;; clj -J-Dclojure.server.repl="{:port 5555 :accept clojure.core.server/io-prepl}"
  (loop [result nil]
    (cond
      (.ready r)
      (recur
        ((fnil conj []) result
          (edn/read-string
            (.readLine r))))

      :else
      result))

  )
