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
    WorkspaceSymbolParams)

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

(defn complete ^CompletableFuture [value]
  (CompletableFuture/completedFuture value))

(deftype  NightincodeDocumentService []
  TextDocumentService

  ;; The Completion request is sent from the client to the server to compute completion items at a given cursor position.
  ;; Completion items are presented in the IntelliSense user interface.
  ;; If computing complete completion items is expensive servers can additional provide
  ;; a handler for the resolve completion item request.
  ;; This request is sent when a completion item is selected in the user interface.
  (^CompletableFuture completion [_ ^CompletionParams _params]
   (complete (Either/forLeft [(doto (CompletionItem.)
                                (.setInsertText "(map )")
                                (.setLabel "clojure.core.map")
                                (.setKind (CompletionItemKind/Function))
                                (.setDetail "Bla bla..."))]))))

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

      (reset! state-ref {:TextDocumentService (NightincodeDocumentService.)
                         :WorkspaceService (NightincodeWorkspaceService.)})

      (complete initializer)))

  (getTextDocumentService [_]
    (:TextDocumentService @state-ref))

  (getWorkspaceService [_]
    (:WorkspaceService @state-ref))

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
  ;;  * remoteInterface - an interface on which RPC methods are looked up
  ;;  * in - input stream to listen for incoming messages
  ;;  * out - output stream to send outgoing messages
  (LSPLauncher/createServerLauncher server System/in System/out))

(defn start []
  ;; Create a new Launcher for a given local service object,
  ;; a given remote interface and an input and output stream.
  ;;
  ;; Parameters:
  ;;  * localService - the object that receives method calls from the remote service
  ;;  * remoteInterface - an interface on which RPC methods are looked up
  ;;  * in - input stream to listen for incoming messages
  ;;  * out - output stream to send outgoing messages
  (let [^LanguageServer server (NightincodeServer.)

        ^Launcher launcher (launcher {:server server})

        ^LanguageClient client (.getRemoteProxy launcher)]

    (.connect ^LanguageClientAware server client)

    (.startListening launcher)))

(defn -main [& _]
  (start))


(comment

  (def server (NightincodeServer.))

  (def server-launcher (launcher {:server server}))

  (.connect ^LanguageClientAware server (.getRemoteProxy server-launcher))

  ;; Start a thread that listens to the input stream. The thread terminates when the stream is closed.
  (.startListening server-launcher)

  )
