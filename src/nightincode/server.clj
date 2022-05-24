(ns nightincode.server
  (:import
   (java.util.concurrent
    CompletableFuture)

   (org.eclipse.lsp4j
    InitializeResult
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
    WillSaveTextDocumentParams)

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

(deftype  NightincodeDocumentService []
  TextDocumentService

  ;; The Completion request is sent from the client to the server to compute completion items at a given cursor position.
  ;; Completion items are presented in the IntelliSense user interface.
  ;; If computing complete completion items is expensive servers can additional provide
  ;; a handler for the resolve completion item request.
  ;; This request is sent when a completion item is selected in the user interface.
  (^CompletableFuture completion [_ ^CompletionParams _params]
   (completed (Either/forLeft [(doto (CompletionItem.)
                                 (.setInsertText "(map )")
                                 (.setLabel "clojure.core.map")
                                 (.setKind (CompletionItemKind/Function))
                                 (.setDetail "Bla bla..."))])))

  ;; The document open notification is sent from the client to the server to signal newly opened text documents.
  ;; The document's truth is now managed by the client and the server must not try to read the document's truth using the document's uri.
  (^void didOpen [_ ^DidOpenTextDocumentParams _params])

  ;; The document change notification is sent from the client to the server to signal changes to a text document.
  (^void didChange [_ ^DidChangeTextDocumentParams _params])

  ;; The document close notification is sent from the client to the server when the document got closed in the client.
  ;; The document's truth now exists where the document's uri points to (e.g. if the document's uri is a file uri the truth now exists on disk).
  (^void didClose [_ ^DidCloseTextDocumentParams _params])

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

      (completed initializer)))

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
    (System/exit (or (:exit-status-code @state-ref) -1)))

  LanguageClientAware
  (^void connect [_ ^LanguageClient client]
   (swap! state-ref assoc :LanguageClient client)

   nil))

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
   (let [^Launcher launcher (launcher {:server server})]
     (.startListening launcher))))

(defn -main [& _]
  (start))


(comment

  (def server (NightincodeServer.))

  (def server-launcher (launcher {:server server}))

  ;; Start a thread that listens to the input stream. The thread terminates when the stream is closed.
  (.startListening server-launcher)

  )
