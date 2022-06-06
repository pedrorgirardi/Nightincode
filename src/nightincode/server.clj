(ns nightincode.server
  (:require
   [clojure.core.server :refer [start-server]]
   [clojure.java.io :as io]
   [clojure.data.json :as json]
   [clojure.string :as str]

   [clj-kondo.core :as clj-kondo]
   [language-server-protocol.core :as lsp])

  (:import
   (java.net
    ServerSocket
    URI))

  (:gen-class))

;; The compiler will emit warnings when reflection is needed
;; to resolve Java method calls or field accesses.
(set! *warn-on-reflection* true)

(def state-ref (atom nil))

(defn text-document-uri [textDocument]
  (:uri textDocument))

(defn text-document-path [textDocument]
  (.getPath (URI. (text-document-uri textDocument))))

(defn analyze-document
  ([textDocument]
   (analyze-document
     {:analysis
      {:arglists true
       :locals true
       :keywords true
       :java-class-usages true}
      :output
      {:canonical-paths true}}
     textDocument))
  ([config textDocument]
   ;; TextDocumentPositionParams
   ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentPositionParams
   (clj-kondo/run!
     {:lint [(text-document-path textDocument)]
      :config config})))

(defn clojuredocs
  "ClojureDocs.org database."
  []
  (json/read (io/reader (io/resource "clojuredocs.json")) :key-fn keyword))

(defn clojuredocs-completion []
  (into []
    (comp
      (filter
        (fn [m]
          (= (:ns m) "clojure.core")))
      (map
        (fn [m]
          (let [completion-item-kind {"var" 6
                                      "function" 3
                                      "macro" 3}

                detail-name (str (:ns m) "/" (:name m))
                detail-arglists (str/join "\n" (map #(str "[" % "]") (:arglists m)))
                detail-docstring (or (:doc m) "")

                detail (str/join "\n\n" [detail-name detail-arglists detail-docstring])]

            {:label (:name m)
             :kind (completion-item-kind (:type m))
             :detail detail}))))
    (:vars (clojuredocs))))

(def clojuredocs-completion-delay
  (delay (clojuredocs-completion)))

(defmethod lsp/handle "initialize" [jsonrpc]

  ;; The initialize request is sent as the first request from the client to the server.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize

  (lsp/response jsonrpc
    {:capabilities
     {;; Defines how the host (editor) should sync document changes to the language server.
      ;;
      ;; 0: Documents should not be synced at all.
      ;; 1: Documents are synced by always sending the full content of the document.
      ;; 2: Documents are synced by sending the full content on open.
      ;;    After that only incremental updates to the document are send.
      ;;
      ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentSyncKind
      :textDocumentSync 1
      :hoverProvider true
      :completionProvider {}}

     :serverInfo
     {:name "Nightincode"}}))

(defmethod lsp/handle "textDocument/didOpen" [jsonrpc]

  ;; The document open notification is sent from the client to the server to signal newly opened text documents.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didOpen

  (let [textDocument (get-in jsonrpc [:params :textDocument])

        result (analyze-document textDocument)
        result (select-keys result [:findings :analysis :summary])]

    (swap! state-ref assoc-in [:nightincode/index (text-document-uri textDocument) :clj-kondo/result] result)))

(defmethod lsp/handle "textDocument/completion" [jsonrpc]

  ;; The Completion request is sent from the client to the server to compute completion items at a given cursor position.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion

  (lsp/response jsonrpc @clojuredocs-completion-delay))

(defn start [config]
  (let [socket-port (with-open [socket (ServerSocket. 0)]
                      (.getLocalPort socket))]
    (start-server
      {:name "REPL"
       :port socket-port
       :accept 'clojure.core.server/repl})

    (doto
      (Thread.
        (fn []
          (lsp/start (select-keys config [:in :out]))))
      (.start))

    (swap! state-ref assoc
      :REPL/port socket-port)))

(defn -main [& _]
  (start
    {:in System/in
     :out System/out}))

