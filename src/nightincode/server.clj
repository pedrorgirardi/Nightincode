(ns nightincode.server
  (:require
   [clojure.core.server :refer [start-server]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [clojure.java.shell :as shell]
   [clojure.stacktrace :as stacktrace]
   [clojure.spec.alpha :as s]
   [clojure.pprint :as pprint]
   [clojure.tools.deps :as deps]

   [clj-kondo.core :as clj-kondo]
   [datascript.core :as d]
   [exoscale.lingo :as lingo]

   [nightincode.specs]
   [nightincode.lsp :as lsp]
   [nightincode.analyzer :as analyzer])

  (:import
   (java.util
    Timer
    TimerTask)

   (java.util.concurrent
    ScheduledExecutorService
    Executors
    TimeUnit)

   (java.io
    Writer
    PrintWriter)

   (java.net
    ServerSocket
    URI))

  (:gen-class))

;; The compiler will emit warnings when reflection is needed
;; to resolve Java method calls or field accesses.
(set! *warn-on-reflection* true)

(defn debounce
  "Copied from https://gist.github.com/oliyh/0c1da9beab43766ae2a6abc9507e732a."
  ([f] (debounce f 1000))
  ([f delay]
   (let [timer (Timer.)
         task (atom nil)]
     (fn [& args]
       (when-let [t @task]
         ;; Cancels this timer task. 
         ;; If the task has been scheduled for one-time execution and has not yet run, 
         ;; or has not yet been scheduled, it will never run. 
         (.cancel ^TimerTask t)

         ;; Removes all cancelled tasks from this timer's task queue. 
         ;; Calling this method has no effect on the behavior of the timer, 
         ;; but eliminates the references to the cancelled tasks from the queue.
         (.purge ^Timer timer))

       (let [new-task (proxy [TimerTask] []
                        (run []
                          (apply f args)))]

         (reset! task new-task)

         ;; Schedules the specified task for execution after the specified delay.
         (.schedule timer new-task ^long delay))))))

(def default-clj-kondo-config
  {:analysis
   {:arglists true
    :locals true
    :keywords true
    :java-class-usages true}
   :output
   {:canonical-paths true}})

(defn text-document-uri [textDocument]
  (:uri textDocument))

(defn text-document-version
  "An identifier to denote a specific version of a text document.
  This information usually flows from the client to the server.

  https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#versionedTextDocumentIdentifier"
  [textDocument]
  (:version textDocument))

(defn text-document-path [textDocument]
  (.getPath (URI. (text-document-uri textDocument))))

(defn text-document-text [textDocument]
  (:text textDocument))

(defn filepath-uri ^URI [filepath]
  (->  (io/file filepath) .toPath .toUri))

(defn uri-path [uri]
  (let [^URI uri (cond
                   (instance? URI uri)
                   uri

                   (string? uri)
                   (URI. uri)

                   :else
                   (throw (ex-info (format "Can't convert %s to java.net.URI." uri) {})))]
    (.getPath uri)))

(defn diagnostic
  "Returns a Diagnostic for a clj-kondo finding.

  https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic"
  [{:keys [row
           end-row
           col
           end-col
           message
           level]}]
  {:range
   {:start
    {:line (dec row)
     :character (dec col)}
    :end
    {:line (dec (or end-row row))
     :character (dec (or end-col col))}}
   :source "Nightincode"
   :message message
   :severity
   (case level
     :error
     1
     :warning
     2

     3)})

(defn uri->diagnostics
  "Returns a map of URI to Diagnostics.

  https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic"
  [findings]
  (group-by
    (comp :uri meta)
    (into []
      (comp
        (filter
          (fn [finding]
            (s/valid? :clj-kondo/finding finding)))
        (map
          (fn [{:keys [filename] :as finding}]
            (let [uri (URI. filename)
                  file? (= (.getScheme uri) "file")]
              (with-meta (diagnostic finding)
                ;; Unsaved files don't have a "file" scheme,
                ;; and it should not be converted to a URI.
                {:uri
                 (if file?
                   (.toString uri)
                   filename)})))))
      findings)))

(defn semthetic-markdown
  [semthetic]
  (let [{:semthetic/keys [semantic modifier doc]} semthetic

        semantic+modifier [semantic modifier]]
    (cond
      (= semantic+modifier [:namespace :def])
      (let [markdown (name (:namespace/name semthetic))
            markdown (if doc
                       (str markdown "\n\n" doc)
                       markdown)]
        markdown)

      (= semantic+modifier [:namespace :usage])
      (name (:namespace-usage/name semthetic))

      (= semantic+modifier [:var :def])
      (let [markdown (format "%s/**%s**" (:var/ns semthetic) (:var/name semthetic))
            markdown (if-let [args (:var/arglist-strs semthetic)]
                       (format "%s\n```clojure\n%s\n```" markdown (str/join "\n" args))
                       markdown)
            markdown (if doc
                       (str markdown "\n\n" doc)
                       markdown)]
        markdown)

      (= semantic+modifier [:var :usage])
      (when-let [to (:var-usage/to semthetic)]
        (symbol (name to) (name (:var-usage/name semthetic))))

      (= semantic+modifier [:local :def])
      (name (:local/name semthetic))

      (= semantic+modifier [:local :usage])
      (name (:local-usage/name semthetic))

      (= semantic+modifier [:keyword :usage])
      (name (:keyword/name semthetic)))))

(defn semthetic-label
  ([semthetic]
   (semthetic-label semthetic {:show-var-namespace? true}))
  ([semthetic options]
   (let [{:semthetic/keys [semantic modifier]} semthetic

         {:keys [show-var-namespace?]} options

         semantic+modifier [semantic modifier]]
     (cond
       (= semantic+modifier [:namespace :def])
       (name (:namespace/name semthetic))

       (= semantic+modifier [:namespace :usage])
       (name (:namespace-usage/name semthetic))

       (= semantic+modifier [:var :def])
       (if show-var-namespace?
         (str (:semthetic/identifier semthetic))
         (name (:var/name semthetic)))

       (= semantic+modifier [:var :usage])
       (str (:semthetic/identifier semthetic))

       (= semantic+modifier [:local :def])
       (name (:local/name semthetic))

       (= semantic+modifier [:local :usage])
       (name (:local-usage/name semthetic))

       (= semantic+modifier [:keyword :usage])
       (str (:semthetic/identifier semthetic))))))

(defn semthetic-symbol-kind
  "Mapping of a Semthetic to a Symbol Kind.

  https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#symbolKind"
  [{:semthetic/keys [semantic]}]
  (case semantic
    :namespace
    3

    8))

(defn semthetic-symbol-range
  "Mapping of a Semthetic to a Range.

  Range encodes the location of a Semthetic symbol.

  https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#range"
  [{:semthetic/keys [semantic modifier] :as semthetic}]
  (let [semantic+modifier [semantic modifier]]
    (cond
      (= semantic+modifier [:namespace :def])
      {:start
       {:line (dec (:namespace/name-row semthetic))
        :character (dec (:namespace/name-col semthetic))}
       :end
       {:line (dec (:namespace/name-row semthetic))
        :character (dec (:namespace/name-end-col semthetic))}}

      (= semantic+modifier [:var :def])
      {:start
       {:line (dec (:var/name-row semthetic))
        :character (dec (:var/name-col semthetic))}
       :end
       {:line (dec (:var/name-row semthetic))
        :character (dec (:var/name-end-col semthetic))}})))


;; ---------------------------------------------------------

(defn mkdir-clj-kondo-cache! [root-path]
  (let [dir (io/file root-path ".clj-kondo")]
    (when-not (.exists dir)
      (.mkdir dir))))

(defn deps-paths
  "Returns a vector of paths and extra-paths."
  [deps-map]
  (reduce-kv
    (fn [paths _ {:keys [extra-paths]}]
      (into paths extra-paths))
    (:paths deps-map)
    (:aliases deps-map)))

(defn analyze-classpath
  "Analyze classpath with clj-kondo."
  [{:keys [root-path]}]
  (with-open [noop-out (PrintWriter. (Writer/nullWriter))
              noop-err (PrintWriter. (Writer/nullWriter))]
    (binding [*out* noop-out
              *err* noop-err]

      ;; Note:
      ;; Analysis doesn't work without a `.clj-kondo` directory.
      (mkdir-clj-kondo-cache! root-path)

      ;; Analyze/lint classpath:
      (when-let [deps-map (deps/slurp-deps (io/file root-path "deps.edn"))]
        (let [basis (deps/create-basis {:projet deps-map})]
          (clj-kondo/run!
            {:lint (:classpath-roots basis)
             :parallel true
             :config
             {:analysis
              {:var-usages false
               :var-definitions {:shallow true}
               :arglists true
               :keywords true
               :java-class-definitions false}
              :output
              {:canonical-paths true}}}))))))

(defn analyze-paths
  "Analyze paths with clj-kondo."
  [{:keys [root-path]}]
  (with-open [noop-out (PrintWriter. (Writer/nullWriter))
              noop-err (PrintWriter. (Writer/nullWriter))]
    (binding [*out* noop-out
              *err* noop-err]

      ;; Note:
      ;; Analysis doesn't work without a `.clj-kondo` directory.
      (mkdir-clj-kondo-cache! root-path)

      ;; Analyze/lint paths and extra-paths:
      (when-let [deps-map (deps/slurp-deps (io/file root-path "deps.edn"))]
        (when-let [paths (deps-paths deps-map)]
          (clj-kondo/run!
            {:lint paths
             :parallel true
             :config default-clj-kondo-config}))))))

(defn analyze-text
  "Analyze `text` with clj-kondo.

  `uri` is used to report the filename."
  [{:keys [uri text]}]
  (with-open [noop-out (PrintWriter. (Writer/nullWriter))
              noop-err (PrintWriter. (Writer/nullWriter))]
    (binding [*out* noop-out
              *err* noop-err]
      (with-in-str text
        (clj-kondo/run!
          {:lint ["-"]
           :filename (or (uri-path uri) uri)
           :config default-clj-kondo-config})))))

(defn config [root-path]
  (let [config-file (io/file root-path "nightincode.edn")]
    (when (.exists config-file)
      (edn/read-string (slurp config-file)))))

(defn analyzer-paths [config root-path]
  (let [paths (get-in config [:analyzer :paths])]
    (map #(.getPath (io/file root-path %)) paths)))


;; ---------------------------------------------------------

;; -- Functions to read and write from and to state

(def state-ref (atom nil))

(defn set-state [f & args]
  (swap! state-ref #(apply f % args)))

(defn _out ^Writer [state]
  (:nightincode/out state))

(defn _repl-port [state]
  (when-let [^ServerSocket server-socket (:nightincode/repl-server-socket state)]
    (.getLocalPort server-socket)))

(defn _root-path [state]
  (get-in state [:lsp/InitializeParams :rootPath]))

(defn _analyzer-conn-paths [state]
  (get-in state [:nightincode/analyzer :conn-paths]))

(defn _analyzer-conn-classpath [state]
  (get-in state [:nightincode/analyzer :conn-classpath]))

(defn _diagnostics [state]
  (:nightincode/diagnostics state))


;; ---------------------------------------------------------


;; -- MessageType
;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#messageType

(def MessageType_Error 1)
(def MessageType_Warning 2)
(def MessageType_Info 3)
(def MessageType_Log 4)
(def MessageType_Debug 5)

(defn textDocument-publishDiagnostics
  "Diagnostics notification are sent from the server to the client to signal results of validation runs.

  https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_publishDiagnostics"
  [out params]
  (lsp/write out
    {:jsonrpc "2.0"
     :method "textDocument/publishDiagnostics"
     :params params}))

(defn window-showMessage
  "The show message notification is sent from a server to a client
  to ask the client to display a particular message in the user interface.

  https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#window_showMessage"
  [out params]
  (lsp/write out
    {:jsonrpc "2.0"
     :method "window/showMessage"
     :params params}))


;; ---------------------------------------------------------


(def process-text-change
  (debounce
    (fn [{:keys [conn out uri text]}]
      (try
        (let [{:keys [analysis findings]} (analyze-text {:uri uri
                                                         :text text})

              index (analyzer/index analysis)

              ;; Note:
              ;; Retract file entities before adding new ones.
              ;; Old Semthetics are retracted because `:file/semthetics` is a component.
              ;;
              ;; TODO: Investigate if there's a better way to update - retract & add.

              tx-data (into []
                        (map
                          (fn [[filename _]]
                            [:db/retractEntity [:file/path filename]]))
                        index)

              tx-data (into tx-data (analyzer/prepare-semthetics index))

              diagnostics (uri->diagnostics findings)]

          (log/info (str "\n" (with-out-str (pprint/pprint diagnostics))))

          (d/transact! conn tx-data)

          (cond
            (seq diagnostics)
            (do
              (set-state assoc
                :nightincode/diagnostics (merge (_diagnostics @state-ref) diagnostics))

              (doseq [[uri diagnostics] diagnostics]
                (textDocument-publishDiagnostics (_out @state-ref)
                  {:uri uri
                   :diagnostics diagnostics})))

            ;; The server must 'clear' diagnostics for `uri`:
            ;; - server's state must be updated to reset diagnostics for `uri`
            ;; - server must publish diagnostics to 'clear' client side diagnostics for `uri`
            :else
            (do
              (set-state update
                :nightincode/diagnostics dissoc uri)

              (textDocument-publishDiagnostics (_out @state-ref)
                {:uri uri
                 :diagnostics []}))))

        (catch Exception ex
          (log/error ex "Processing text change error. Document:" uri))))
    300))


;; ---------------------------------------------------------


(defmethod lsp/handle "initialize" [request]

  ;; The initialize request is sent as the first request from the client to the server.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#serverCapabilities

  (let [root-path (get-in request [:params :rootPath])

        config (config root-path)

        capabilities (or (:capabilities config)
                       {;; Defines how the host (editor) should sync document changes to the language server.
                        ;;
                        ;; 0: Documents should not be synced at all.
                        ;; 1: Documents are synced by always sending the full content of the document.
                        ;; 2: Documents are synced by sending the full content on open.
                        ;;    After that only incremental updates to the document are send.
                        ;;
                        ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentSyncKind
                        :textDocumentSync 1

                        ;; Goto Definition Request
                        ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_definition
                        :definitionProvider true

                        ;; Find References Request
                        ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_references
                        :referencesProvider true

                        ;; Document Highlights Request
                        ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_documentHighlight
                        :documentHighlightProvider true

                        ;; Hover Request
                        ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_hover
                        :hoverProvider true

                        ;; Document Symbols Request
                        ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_documentSymbol
                        :documentSymbolProvider true

                        ;; Workspace Symbols Request
                        ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_symbol 
                        :workspaceSymbolProvider true})

        conn-paths (d/create-conn analyzer/schema)

        conn-classpath (d/create-conn analyzer/schema)

        ;; Analyze & transact Semthetics:
        ;; (If there's a deps.edn at root-path.)
        {:keys [nightincode/diagnostics]} (when-let [result (analyze-paths {:root-path root-path})]
                                            (let [{:keys [findings analysis]} result

                                                  index (analyzer/index analysis)

                                                  tx-data (analyzer/prepare-semthetics index)]

                                              (merge {:tx-report (d/transact! conn-paths tx-data)}
                                                (when-let [diagnostics (uri->diagnostics findings)]
                                                  {:nightincode/diagnostics diagnostics}))))

        initialized (set-state assoc
                      :lsp/InitializeParams (:params request)
                      :nightincode/analyzer {:conn-paths conn-paths :conn-classpath conn-classpath}
                      :nightincode/diagnostics diagnostics)]

    ;; -- Analyze classpath on a separate thread
    (future
      (let [{:keys [analysis]} (analyze-classpath {:root-path root-path})
            index (analyzer/index analysis)
            semthetics (analyzer/prepare-semthetics index)]
        (d/transact! conn-classpath semthetics)))

    (when-not (s/valid? :nightincode.state/initialized initialized)
      (log/warn (lingo/explain-str :nightincode.state/initialized initialized)))

    (lsp/response request
      {:capabilities capabilities
       :serverInfo
       {:name "Nightincode"
        :version "0.12.0-dev"}})))

(defmethod lsp/handle "initialized" [notification]

  ;; The initialized notification is sent from the client to the server after
  ;; the client received the result of the initialize request but before
  ;; the client is sending any other request or notification to the server.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialized

  (let [probe-state (when-let [pid (get-in @state-ref [:lsp/InitializeParams :processId])]

                      ;; Nightincode needs to check frequently if the parent process is still alive.
                      ;; A client e.g. Visual Studio Code should ask the server to exit, but that might not happen.
                      ;;
                      ;; > The shutdown request is sent from the client to the server.
                      ;;   It asks the server to shut down, but to not exit (otherwise the response might not be delivered correctly to the client).
                      ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#shutdown
                      ;;
                      ;; > A notification to ask the server to exit its process.
                      ;;   The server should exit with success code 0 if the shutdown request has been received before; otherwise with error code 1.
                      ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#exit

                      (let [^ScheduledExecutorService executor (Executors/newScheduledThreadPool 1)

                            probe-delay 15

                            probe (.scheduleWithFixedDelay executor
                                    (fn []
                                      ;; Checking if a process is alive was copied from:
                                      ;; https://github.com/clojure-lsp/lsp4clj/blob/492c075d145ddb4e46e245a6f7a4de8a4670fe72/server/src/lsp4clj/core.clj#L296
                                      ;;
                                      ;; Thanks, lsp4clj!

                                      (let [windows? (str/includes? (System/getProperty "os.name") "Windows")

                                            process-alive? (cond
                                                             windows?
                                                             (let [{:keys [out]} (shell/sh "tasklist" "/fi" (format "\"pid eq %s\"" pid))]
                                                               (str/includes? out (str pid)))

                                                             :else
                                                             (let [{:keys [exit]} (shell/sh "kill" "-0" (str pid))]
                                                               (zero? exit)))]

                                        (when-not process-alive?
                                          (log/debug (format "Parent process %s no longer exists; Exiting server..." pid))

                                          (System/exit 1))))
                                    probe-delay
                                    probe-delay
                                    TimeUnit/SECONDS)]
                        {:nightincode/probe-executor executor
                         :nightincode/probe probe}))]

    (set-state merge {:lsp/InitializedParams (:params notification)} probe-state)

    ;; Log a welcome message in the client:
    (lsp/write (_out @state-ref)
      {:jsonrpc "2.0"
       :method "window/logMessage"
       :params
       {:type 4
        :message (format "Nightincode is up and running!\n\nA REPL is available on port %s.\n\nHappy coding!" (_repl-port @state-ref))}})    

    ;; Publish diagnostics:
    (doseq [[uri diagnostics] (_diagnostics @state-ref)]
      (textDocument-publishDiagnostics (_out @state-ref)
        {:uri uri
         :diagnostics diagnostics}))))


(defmethod lsp/handle "shutdown" [request]

  ;; The shutdown request is sent from the client to the server.
  ;; It asks the server to shut down, but to not exit (otherwise the response might not be delivered correctly to the client).
  ;; There is a separate exit notification that asks the server to exit.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#shutdown

  (log/debug "Request to shutdown")

  (set-state assoc :nightincode/shutdown? true)

  (lsp/response request nil))

(defmethod lsp/handle "exit" [_]

  ;; A notification to ask the server to exit its process.
  ;; The server should exit with success code 0 if the shutdown request has been received before; otherwise with error code 1.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#exit

  (log/debug "Notification to exit")

  (System/exit (if (:nightincode/shutdown? @state-ref)
                 0
                 1)))

(defmethod lsp/handle "$/cancelRequest" [_]

  ;; The base protocol offers support for request cancellation.
  ;;
  ;; A request that got canceled still needs to return from the server and send a response back.
  ;; It can not be left open / hanging. This is in line with the JSON-RPC protocol that requires
  ;; that every request sends a response back.
  ;;
  ;; In addition it allows for returning partial results on cancel.
  ;;
  ;; If the request returns an error response on cancellation it is advised
  ;; to set the error code to ErrorCodes.RequestCancelled.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#cancelRequest

  nil)

(defmethod lsp/handle "$/setTrace" [_]

  ;; A notification that should be used by the client to modify the trace setting of the server.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#setTrace

  nil)


(defmethod lsp/handle "textDocument/didOpen" [notification]

  ;; The document open notification is sent from the client to the server to signal newly opened text documents.
  ;; The document’s content is now managed by the client and the server must not try to read the document’s content using the document’s Uri.
  ;; Open in this sense means it is managed by the client. It doesn’t necessarily mean that its content is presented in an editor.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didOpen

  (try
    (let [textDocument (get-in notification [:params :textDocument])

          text-document-uri (text-document-uri textDocument)
          text-document-text (text-document-text textDocument)]

      ;; Persist in-memory document's text.
      (set-state assoc-in [:nightincode/document-index text-document-uri] {:text text-document-text})

      ;; Text change processing and diagnostics is debounced.
      (process-text-change
        {:uri text-document-uri
         :text text-document-text
         :conn (_analyzer-conn-paths @state-ref)
         :out (_out @state-ref)}))

    (catch Exception ex
      (log/error ex "Error: textDocument/didOpen"))))

(defmethod lsp/handle "textDocument/didChange" [notification]

  ;; The document change notification is sent from the client to the server to signal changes to a text document.
  ;; Before a client can change a text document it must claim ownership of its content using the textDocument/didOpen notification.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didChange

  (try
    (let [textDocument (get-in notification [:params :textDocument])

          text-document-uri (text-document-uri textDocument)

          text-document-version (text-document-version textDocument)

          ;; The client sends the full text because textDocumentSync capability is set to 1 (full).
          text-document-text (get-in notification [:params :contentChanges 0 :text])]

      ;; Update document's persisted text.
      (set-state assoc-in [:nightincode/document-index text-document-uri] {:text text-document-text
                                                                           :version text-document-version})

      ;; Text change processing and diagnostics is debounced.
      (process-text-change
        {:uri text-document-uri
         :text text-document-text
         :conn (_analyzer-conn-paths @state-ref)
         :out (_out @state-ref)}))

    (catch Exception ex
      (log/error ex "Error: textDocument/didChange"))))

(defmethod lsp/handle "textDocument/didSave" [_notification]

  ;; The document save notification is sent from the client to the server when the document was saved in the client.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didSave

  nil)

(defmethod lsp/handle "textDocument/didClose" [notification]

  ;; The document close notification is sent from the client to the server when the document got closed in the client.
  ;; The document’s master now exists where the document’s Uri points to (e.g. if the document’s Uri is a file Uri the master now exists on disk).
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didClose

  (try
    (let [textDocument (get-in notification [:params :textDocument])

          ;; Note: URI of unsaved files is like "untitled:Untitled-1".
          text-document-uri (text-document-uri textDocument)]

      ;; Clear document's persisted text.
      (set-state update :nightincode/document-index dissoc text-document-uri))

    (catch Exception ex
      (log/error ex "Error: textDocument/didClose"))))

(defmethod lsp/handle "textDocument/definition" [request]

  ;; The go to definition request is sent from the client to the server
  ;; to resolve the definition location of a symbol at a given text document position.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_definition

  (try
    (let [textDocument (get-in request [:params :textDocument])

          cursor-line (get-in request [:params :position :line])
          cursor-character (get-in request [:params :position :character])

          db (d/db (_analyzer-conn-paths @state-ref))
          dbc (d/db (_analyzer-conn-classpath @state-ref))

          cursor-semthetic (analyzer/?cursor-semthetic db
                             {:filename (text-document-path textDocument)
                              :row (inc cursor-line)
                              :col (inc cursor-character)
                              :col-end (inc cursor-character)})

          locations (mapcat
                      (fn [{:semthetic/keys [filename locs]}]
                        (let [jar? (str/includes? filename ".jar")

                              [jar f] (str/split filename #":")

                              filename (cond
                                         jar?
                                         (with-open [zf (java.util.zip.ZipFile. ^String jar)]
                                           (reduce
                                             (fn [_ ^java.util.zip.ZipEntry e]
                                               (when (= (.getName e) f)
                                                 (let [tmp (io/file (System/getProperty "java.io.tmpdir") (.getName e))]
                                                   
                                                   ;; Creates parent directories:
                                                   (io/make-parents tmp)

                                                   (with-open [in (.getInputStream zf e)]
                                                     (io/copy in tmp))

                                                   (reduced (.getPath tmp)))))
                                             nil
                                             (enumeration-seq (.entries zf))))

                                         :else
                                         filename)]
                          (map
                            (fn [loc]
                              (analyzer/loc-location filename loc))
                            locs)))
                      (or
                        (seq (analyzer/?semantic-definitions db cursor-semthetic))
                        (seq (analyzer/?semantic-definitions dbc cursor-semthetic))))]

      (lsp/response request (seq locations)))

    (catch Exception ex
      (log/error ex "Error: textDocument/definition")

      (lsp/error-response request
        {:code -32803
         :message
         (format "Nightincode failed to find definitions. (%s)\n"
           (with-out-str (stacktrace/print-stack-trace ex)))}))))

(defmethod lsp/handle "textDocument/references" [request]

  ;; The references request is sent from the client to the server
  ;; to resolve project-wide references for the symbol denoted by the given text document position.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_references

  (try
    (let [textDocument (get-in request [:params :textDocument])

          cursor-line (get-in request [:params :position :line])
          cursor-character (get-in request [:params :position :character])

          db (d/db (_analyzer-conn-paths @state-ref))

          cursor-semthetic (analyzer/?cursor-semthetic db
                             {:filename (text-document-path textDocument)
                              :row (inc cursor-line)
                              :col (inc cursor-character)
                              :col-end (inc cursor-character)})

          locations (mapcat
                      (fn [{:semthetic/keys [filename locs]}]
                        (map
                          (fn [loc]
                            (analyzer/loc-location filename loc))
                          locs))
                      (analyzer/?semantic-usages db cursor-semthetic))]

      (lsp/response request (seq locations)))

    (catch Exception ex
      (log/error ex "Error: textDocument/references")

      (lsp/error-response request
        {:code -32803
         :message
         (format "Nightincode failed to find references. (%s)\n"
           (with-out-str (stacktrace/print-stack-trace ex)))}))))

(defmethod lsp/handle "textDocument/documentHighlight" [request]

  ;; The document highlight request is sent from the client to the server
  ;; to resolve a document highlights for a given text document position.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_documentHighlight

  (try
    (let [textDocument (get-in request [:params :textDocument])

          cursor-line (get-in request [:params :position :line])
          cursor-character (get-in request [:params :position :character])

          db (d/db (_analyzer-conn-paths @state-ref))

          cursor-semthetic (analyzer/?cursor-semthetic db
                             {:filename (text-document-path textDocument)
                              :row (inc cursor-line)
                              :col (inc cursor-character)
                              :col-end (inc cursor-character)})

          definitions-highlights (mapcat
                                   (fn [{:semthetic/keys [locs]}]
                                     (map
                                       (fn [loc]
                                         {:range (analyzer/loc-range loc)})
                                       locs))
                                   (analyzer/?semantic-file-definitions db cursor-semthetic))

          usages-highlights (mapcat
                              (fn [{:semthetic/keys [locs]}]
                                (map
                                  (fn [loc]
                                    {:range (analyzer/loc-range loc)})
                                  locs))
                              (analyzer/?semantic-file-usages db cursor-semthetic))

          highlights (reduce into [] [definitions-highlights
                                      usages-highlights])]

      (lsp/response request (seq highlights)))

    (catch Exception ex
      (log/error ex "Error: textDocument/documentHighlight")

      (lsp/error-response request
        {:code -32803
         :message
         (format "Nightincode failed to find highlights. (%s)\n"
           (with-out-str (stacktrace/print-stack-trace ex)))}))))

(defmethod lsp/handle "textDocument/documentSymbol" [request]

  ;; The document symbol request is sent from the client to the server.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_documentSymbol

  (try
    (let [textDocument (get-in request [:params :textDocument])

          db (d/db (_analyzer-conn-paths @state-ref))

          ;; Find namespace and var definitions in file.
          definitions (d/q '[:find [(pull ?e [*]) ...]
                             :in $ ?filename [?semantic ...]
                             :where
                             [?e :semthetic/filename ?filename]
                             [?e :semthetic/modifier :def]
                             [?e :semthetic/semantic ?semantic]]
                        db (text-document-path textDocument) [:namespace :var])

          symbols (mapcat
                    (fn [{:semthetic/keys [modifier filename locs] :as semthetic}]
                      (map
                        (fn [loc]
                          {:name (or (semthetic-label semthetic {:show-var-namespace? false}) "?")

                           :kind (semthetic-symbol-kind semthetic)

                           ;; The range enclosing this symbol not including leading/trailing whitespace
                           ;; but everything else like comments. This information is typically used to
                           ;; determine if the clients cursor is inside the symbol to reveal in the
                           ;; symbol in the UI.
                           :range (semthetic-symbol-range semthetic)

                           ;; The range that should be selected and revealed when this symbol is being
                           ;; picked, e.g. the name of a function. Must be contained by the `range`.
                           :selectionRange (analyzer/loc-range loc)})
                        locs))
                    definitions)]

      (lsp/response request (seq symbols)))

    (catch Exception ex
      (log/error ex "Error: textDocument/documentSymbol")

      (lsp/error-response request
        {:code -32803
         :message
         (format "Nightincode failed to find document symbols. (%s)\n"
           (with-out-str (stacktrace/print-stack-trace ex)))}))))


(defmethod lsp/handle "textDocument/hover" [request]

  ;; The hover request is sent from the client to the server to request hover information at a given text document position.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_hover

  (try
    (let [textDocument (get-in request [:params :textDocument])

          cursor-position (get-in request [:params :position])

          {cursor-line :line
           cursor-character :character} cursor-position

          db (d/db (_analyzer-conn-paths @state-ref))
          dbc (d/db (_analyzer-conn-classpath @state-ref))

          cursor-semthetic (analyzer/?cursor-semthetic db
                             {:filename (text-document-path textDocument)
                              :row (inc cursor-line)
                              :col (inc cursor-character)
                              :col-end (inc cursor-character)})

          cursor-definitions (analyzer/?semantic-definitions dbc cursor-semthetic)]

      (lsp/response request
        (when (seq cursor-definitions)
          (let [loc (analyzer/cursor-loc cursor-semthetic cursor-position)

                range (analyzer/loc-range loc)

                values (map semthetic-markdown cursor-definitions)

                markdown (str/join "\n\n" values)]

            {:range range
             :contents
             {:kind "markdown"
              :value (or markdown "")}}))))

    (catch Exception ex
      (log/error ex "Error: textDocument/hover")

      (lsp/error-response request
        {:code -32803
         :message
         (format "Nightincode failed to find hover information. (%s)\n"
           (with-out-str (stacktrace/print-stack-trace ex)))}))))


(defmethod lsp/handle "workspace/symbol" [request]

  ;; The workspace symbol request is sent from the client to the server to list project-wide symbols matching the query string.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_symbol

  (try
    (let [db (d/db (_analyzer-conn-paths @state-ref))

          definitions (d/q '[:find [(pull ?e [*]) ...]
                             :in $ [?semantic ...]
                             :where
                             [?e :semthetic/modifier :def]
                             [?e :semthetic/semantic ?semantic]]
                        db [:namespace :var])

          symbols (mapcat
                    (fn [{:semthetic/keys [filename locs] :as semthetic}]
                      (map
                        (fn [loc]
                          {:name (or (semthetic-label semthetic) "?")
                           :kind (semthetic-symbol-kind semthetic)
                           :location (analyzer/loc-location filename loc)})
                        locs))
                    definitions)]

      (lsp/response request (seq symbols)))

    (catch Exception ex
      (log/error ex "Error: workspace/symbol")

      (lsp/error-response request
        {:code -32803
         :message
         (format "Nightincode failed to find workspace symbols. (%s)\n"
           (with-out-str (stacktrace/print-stack-trace ex)))}))))

(defn start [config]
  (let [^ServerSocket server-socket (start-server
                                      {:name "REPL"
                                       :port 0
                                       :accept 'clojure.core.server/repl})]

    (reset! state-ref #:nightincode {:repl-server-socket server-socket
                                     :in (:in config)
                                     :out (:out config)})

    (doto
      (Thread. #(lsp/start config))
      (.start))))

(defn -main [& _]
  (start
    {:in System/in
     :out System/out}))
