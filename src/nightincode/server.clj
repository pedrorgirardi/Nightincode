(ns nightincode.server
  (:require
   [clojure.core.server :refer [start-server]]
   [clojure.java.io :as io]
   [clojure.data.json :as json]
   [clojure.string :as str]

   [clj-kondo.core :as clj-kondo])

  (:import
   (java.util.concurrent
    CompletableFuture)

   (java.net
    ServerSocket
    URI)

   (org.eclipse.lsp4j
    InitializeResult
    InitializedParams
    ServerCapabilities
    TextDocumentSyncKind
    CompletionOptions
    CompletionParams
    CompletionItem
    CompletionItemKind
    WorkspaceSymbolParams
    DidOpenTextDocumentParams
    DidChangeTextDocumentParams
    DidCloseTextDocumentParams
    DidSaveTextDocumentParams
    WillSaveTextDocumentParams
    MessageParams
    MessageType
    TextDocumentItem)

   (org.eclipse.lsp4j.services
    LanguageClient
    LanguageServer
    LanguageClientAware
    TextDocumentService
    WorkspaceService)

   (org.eclipse.lsp4j.jsonrpc
    Launcher)

   (org.eclipse.lsp4j.launch
    LSPLauncher)

   (org.eclipse.lsp4j.jsonrpc.messages
    Either))

  (:gen-class))

;; The compiler will emit warnings when reflection is needed
;; to resolve Java method calls or field accesses.
(set! *warn-on-reflection* true)

(def state-ref (atom nil))

(defn completed ^CompletableFuture [value]
  (CompletableFuture/completedFuture value))

(defn mlog
  "Log MessageParams."
  ^MessageParams [message]
  (MessageParams. MessageType/Log message))


(def clojuredocs
  "ClojureDocs.org database."
  (delay (json/read (io/reader (io/resource "clojuredocs.json")) :key-fn keyword)))

(defn clojuredocs-completion []
  (into []
    (comp
      (filter
        (fn [m]
          (= (:ns m) "clojure.core")))
      (map
        (fn [m]
          (let [completion-item-kind {"var" (CompletionItemKind/Variable)
                                      "function" (CompletionItemKind/Function)
                                      "macro" (CompletionItemKind/Function)}

                detail-name (str (:ns m) "/" (:name m))

                detail-arglists (str/join "\n" (map #(str "[" % "]") (:arglists m)))

                detail-docstring (or (:doc m) "")

                detail (str/join "\n\n" [detail-name detail-arglists detail-docstring])]
            (doto (CompletionItem.)
              (.setInsertText (:name m))
              (.setLabel (:name m))
              (.setKind (completion-item-kind (:type m)))
              (.setDetail detail))))))
    (:vars @clojuredocs)))


(deftype  NightincodeDocumentService []
  TextDocumentService

  ;; The Completion request is sent from the client to the server to compute completion items at a given cursor position.
  ;; Completion items are presented in the IntelliSense user interface.
  ;; If computing complete completion items is expensive servers can additional provide
  ;; a handler for the resolve completion item request.
  ;; This request is sent when a completion item is selected in the user interface.
  (^CompletableFuture completion [_ ^CompletionParams _params]
   (completed (Either/forLeft (clojuredocs-completion))))

  ;; The document open notification is sent from the client to the server to signal newly opened text documents.
  ;; The document's truth is now managed by the client and the server must not try to read the document's truth using the document's uri.
  (^void didOpen [_ ^DidOpenTextDocumentParams params]
   (let [^TextDocumentItem document (.getTextDocument params)

         document-uri (.getUri document)
         document-path (.getPath (URI. document-uri))

         result (clj-kondo/run!
                  {:lint [document-path]
                   :config
                   {:output
                    {:analysis
                     {:arglists true
                      :locals true
                      :keywords true
                      :java-class-usages true}
                     :format :json
                     :canonical-paths true}}})

         result (select-keys result [:findings :analysis :summary])]

     (swap! state-ref assoc-in [:nightincode/index document-uri :clj-kondo/result] result)))

  ;; The document change notification is sent from the client to the server to signal changes to a text document.
  (^void didChange [_ ^DidChangeTextDocumentParams _params])

  ;; The document close notification is sent from the client to the server when the document got closed in the client.
  ;; The document's truth now exists where the document's uri points to (e.g. if the document's uri is a file uri the truth now exists on disk).
  (^void didClose [_ ^DidCloseTextDocumentParams params]
   ;; Dispose index when the document is closed.
   (let [document-uri (.getUri (.getTextDocument params))]
     (swap! state-ref update :nightincode/index dissoc document-uri)))

  ;; The document save notification is sent from the client to the server when the document for saved in the client.
  (^void didSave [_ ^DidSaveTextDocumentParams _params]))

(deftype NightincodeWorkspaceService []
  WorkspaceService

  ;; The workspace symbol request is sent from the client to the server
  ;; to list project-wide symbols matching the query string.
  (^CompletableFuture symbol [_ ^WorkspaceSymbolParams _params]))

(deftype NightincodeServer []
  LanguageServer

  ;; The initialize request is sent as the first request from the client to the server.
  ;; If the server receives requests or notifications before the initialize request, it should act as follows:
  ;;  * for a request, the response should be errored with: ResponseErrorCode.ServerNotInitialized. The message can be picked by the server.
  ;;  * notifications should be dropped, except for the exit notification. This will allow the client to exit a server without an initialize request.
  ;;
  ;; Until the server has responded to the initialize request with an InitializeResult,
  ;; the client must not send any additional requests or notifications to the server.
  ;;
  ;; During the initialize request, the server is allowed to send the notifications
  ;; window/showMessage, window/logMessage, and telemetry/event,
  ;; as well as the request window/showMessageRequest, to the client.
  (initialize [_ _params]
    (let [initializer (InitializeResult. (ServerCapabilities.))

          ^ServerCapabilities capabilities (.getCapabilities initializer)]

      ;; Documents are synced by always sending the full content of the document.
      (.setTextDocumentSync capabilities TextDocumentSyncKind/Full)

      ;; The server provides completion support.
      (.setCompletionProvider capabilities (CompletionOptions.))

      (swap! state-ref assoc :ServerCapabilities capabilities)

      (completed initializer)))

  (^void initialized [_ ^InitializedParams _params]
   (let [{client :LanguageClient
          capabilities :ServerCapabilities
          REPL-port :REPL/port} @state-ref]
     (.logMessage client
       (mlog
         (str "Debug:"
           "\n\t- Socket REPL port: " REPL-port
           "\n\t- Capabilities: " capabilities)))))

  (getTextDocumentService [_]
    (NightincodeDocumentService.))

  (getWorkspaceService [_]
    (NightincodeWorkspaceService.))

  ;; The shutdown request is sent from the client to the server. It asks the server to shutdown,
  ;; but to not exit (otherwise the response might not be delivered correctly to the client).
  ;; There is a separate exit notification that asks the server to exit.
  (shutdown [_]
    (swap! state-ref assoc :exit-status-code 0)

    nil)

  ;; A notification to ask the server to exit its process.
  (exit [_]
    (System/exit (or (:exit-status-code @state-ref) -1))))

(defn launcher ^Launcher [{:keys [server]}]
  ;; Create a new Launcher for a given local service object,
  ;; a given remote interface and an input and output stream.
  ;;
  ;; Parameters:
  ;;  * localService - the object that receives method calls from the remote service
  ;;  * in - input stream to listen for incoming messages
  ;;  * out - output stream to send outgoing messages
  (LSPLauncher/createServerLauncher server System/in System/out))

(defn start
  ([]
   (start {:server (NightincodeServer.)}))
  ([{:keys [server]}]
   (let [^Launcher launcher (launcher {:server server})

         socket-port (with-open [socket (ServerSocket. 0)]
                       (.getLocalPort socket))]

     (start-server
       {:name "REPL"
        :port socket-port
        :accept 'clojure.core.server/repl})

     (swap! state-ref assoc
       :REPL/port socket-port
       :LanguageClient (.getRemoteProxy launcher))

     (.startListening launcher))))

(defn -main [& _]
  (start))


(comment

  (def server (NightincodeServer.))

  (def server-launcher (launcher {:server server}))

  ;; Start a thread that listens to the input stream. The thread terminates when the stream is closed.
  (.startListening server-launcher)

  )
