(ns nightincode.server
  (:require
   [clojure.core.server :refer [start-server]]
   [clojure.java.io :as io]
   [clojure.data.json :as json]
   [clojure.string :as str]
   [clojure.tools.logging :as log]

   [clj-kondo.core :as clj-kondo]
   [lspie.api :as lsp])

  (:import
   (java.io
    Writer)

   (java.net
    ServerSocket
    URI))

  (:gen-class))

;; The compiler will emit warnings when reflection is needed
;; to resolve Java method calls or field accesses.
(set! *warn-on-reflection* true)

(def state-ref (atom nil))

(defn writer ^Writer [state]
  (:nightincode/writer state))

(defn repl-port [state]
  (when-let [^ServerSocket server-socket (:nightincode/repl-server-socket state)]
    (.getLocalPort server-socket)))


;; ---------------------------------------------------------


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

(defmethod lsp/handle "initialize" [request]

  ;; The initialize request is sent as the first request from the client to the server.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#serverCapabilities

  (swap! state-ref assoc :LSP/InitializeParams (:params request))

  (lsp/response request
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
      :completionProvider {}}

     :serverInfo
     {:name "Nightincode"}}))

(defmethod lsp/handle "initialized" [notification]

  ;; The initialized notification is sent from the client to the server after
  ;; the client received the result of the initialize request but before
  ;; the client is sending any other request or notification to the server.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialized

  (swap! state-ref assoc :LSP/InitializedParams (:params notification))

  ;; Log a welcome message in the client.
  (lsp/write (writer @state-ref)
    {:jsonrpc "2.0"
     :method "window/logMessage"
     :params {:type 4
              :message (format "Nightincode is up and running!\n\nA REPL is available on port %s.\n\nHappy coding!" (repl-port @state-ref))}}))

(defmethod lsp/handle "shutdown" [request]

  ;; The shutdown request is sent from the client to the server.
  ;; It asks the server to shut down, but to not exit (otherwise the response might not be delivered correctly to the client).
  ;; There is a separate exit notification that asks the server to exit.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#shutdown

  (swap! state-ref assoc :nightincode/shutdown? true)

  (lsp/response request nil))

(defmethod lsp/handle "exit" [_]

  ;; A notification to ask the server to exit its process.
  ;; The server should exit with success code 0 if the shutdown request has been received before; otherwise with error code 1.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#exit

  (System/exit (if (:nightincode/shutdown? @state-ref)
                 0
                 1)))

(defmethod lsp/handle "textDocument/didOpen" [notification]

  ;; The document open notification is sent from the client to the server to signal newly opened text documents.
  ;; The document’s content is now managed by the client and the server must not try to read the document’s content using the document’s Uri.
  ;; Open in this sense means it is managed by the client. It doesn’t necessarily mean that its content is presented in an editor.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didOpen

  (let [textDocument (get-in notification [:params :textDocument])

        result (analyze-document textDocument)
        result (select-keys result [:findings :analysis :summary])]

    (swap! state-ref assoc-in [:nightincode/index (text-document-uri textDocument) :clj-kondo/result] result)))

(defmethod lsp/handle "textDocument/didClose" [notification]

  ;; The document close notification is sent from the client to the server when the document got closed in the client.
  ;; The document’s master now exists where the document’s Uri points to (e.g. if the document’s Uri is a file Uri the master now exists on disk).
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didClose

  (let [textDocument (get-in notification [:params :textDocument])]
    (swap! state-ref update :nightincode/index dissoc (text-document-uri textDocument))))

(defmethod lsp/handle "textDocument/completion" [request]

  ;; The Completion request is sent from the client to the server to compute completion items at a given cursor position.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion

  (lsp/response request @clojuredocs-completion-delay))


;; ---------------------------------------------------------


(defn start [config]
  (let [^ServerSocket server-socket (start-server
                                      {:name "REPL"
                                       :port 0
                                       :accept 'clojure.core.server/repl})]

    (log/debug "REPL port:" (.getLocalPort server-socket))

    (reset! state-ref #:nightincode {:repl-server-socket server-socket
                                     :reader (:reader config)
                                     :writer (:writer config)})

    (doto
      (Thread. #(lsp/start config))
      (.start))))

(defn -main [& _]
  (start
    {:reader (lsp/buffered-reader System/in)
     :writer (lsp/buffered-writer System/out)
     :trace (fn [{:keys [status content]}]
              ;; Debug: log every message decoded by lspie.
              (when (= status :message-decoded)
                (log/debug (select-keys content [:id :method]))))}))


(comment

  (keys @state-ref)

  (let [{:keys [LSP/InitializeParams
                LSP/InitializedParams

                nightincode/index]} @state-ref]

    (def initialize-params InitializeParams)
    (def initialized-params InitializedParams)
    (def index index))

  (keys index)

  (lsp/write (writer @state-ref)
    {:jsonrpc "2.0"
     :method "window/showMessage"
     :params {:type 3
              :message "Hello!"}})

  )
