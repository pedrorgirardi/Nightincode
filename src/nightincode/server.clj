(ns nightincode.server
  (:require
   [clojure.core.server :refer [start-server]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.data.json :as json]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [clojure.java.shell :as shell]
   [clojure.stacktrace :as stacktrace]

   [clj-kondo.core :as clj-kondo]
   [lspie.api :as lsp]
   [datascript.core :as d]

   [nightincode.analyzer :as analyzer])

  (:import
   (java.util.concurrent
    ScheduledExecutorService
    Executors
    TimeUnit)

   (java.io
    Writer)

   (java.net
    ServerSocket
    URI))

  (:gen-class))

;; The compiler will emit warnings when reflection is needed
;; to resolve Java method calls or field accesses.
(set! *warn-on-reflection* true)


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

(defn text-document-path [textDocument]
  (.getPath (URI. (text-document-uri textDocument))))

(defn text-document-text [textDocument]
  (:text textDocument))

(defn filepath-uri ^URI [filepath]
  (->  (io/file filepath) .toPath .toUri))

(defn semthetic-markdown
  [semthetic]
  (let [{:semthetic/keys [semantic modifier doc]} semthetic

        modifier+semantic [modifier semantic]]
    (cond
      (= modifier+semantic [:namespace :def])
      (let [markdown (name (:namespace/name semthetic))
            markdown (if doc
                       (str markdown "\n\n" doc)
                       markdown)]
        markdown)

      (= modifier+semantic [:namespace :usage])
      (name (:namespace-usage/name semthetic))

      (= modifier+semantic [:var :def])
      (let [markdown (format "%s/**%s**" (:var/ns semthetic) (:var/name semthetic))
            markdown (if-let [args (:var/arglist-strs semthetic)]
                       (format "%s\n```clojure\n%s\n```" markdown (str/join "\n" args))
                       markdown)
            markdown (if doc
                       (str markdown "\n\n" doc)
                       markdown)]
        markdown)

      (= modifier+semantic [:var :usage])
      (when-let [to (:var-usage/to semthetic)]
        (symbol (name to) (name (:var-usage/name semthetic))))

      (= modifier+semantic [:local :def])
      (name (:local/name semthetic))

      (= modifier+semantic [:local :usage])
      (name (:local-usage/name semthetic))

      (= modifier+semantic [:keyword :usage])
      (name (:keyword/name semthetic)))))

(defn semthetic-label
  ([semthetic]
   (semthetic-label semthetic {:show-var-namespace? true}))
  ([semthetic options]
   (let [{:semthetic/keys [semantic modifier]} semthetic

         {:keys [show-var-namespace?]} options

         modifier+semantic [modifier semantic]]
     (cond
       (= modifier+semantic [:namespace :def])
       (name (:namespace/name semthetic))

       (= modifier+semantic [:namespace :usage])
       (name (:namespace-usage/name semthetic))

       (= modifier+semantic [:var :def])
       (if show-var-namespace?
         (str (symbol (name (:var/ns semthetic)) (name (:var/name semthetic))))
         (name (:var/name semthetic)))

       (= modifier+semantic [:var :usage])
       (when-let [to (:var-usage/to semthetic)]
         (symbol (name to) (name (:var-usage/name semthetic))))

       (= modifier+semantic [:local :def])
       (name (:local/name semthetic))

       (= modifier+semantic [:local :usage])
       (name (:local-usage/name semthetic))

       (= modifier+semantic [:keyword :usage])
       (name (:keyword/name semthetic))))))

(defn semthetic-symbol-kind
  "Mapping of a Semthetic to a Symbol Kind.

  https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#symbolKind"
  [{:semthetic/keys [modifier]}]
  (case modifier
    :namespace
    3

    8))

(defn semthetic-symbol-range
  "Mapping of a Semthetic to a Range.

  Range encodes the location of a Semthetic symbol.

  https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#range"
  [{:semthetic/keys [semantic modifier] :as semthetic}]
  (let [modifier+semantic [modifier semantic]]
    (cond
      (= modifier+semantic [:namespace :def])
      {:start
       {:line (dec (:namespace/name-row semthetic))
        :character (dec (:namespace/name-col semthetic))}
       :end
       {:line (dec (:namespace/name-row semthetic))
        :character (dec (:namespace/name-end-col semthetic))}}

      (= modifier+semantic [:var :def])
      {:start
       {:line (dec (:var/name-row semthetic))
        :character (dec (:var/name-col semthetic))}
       :end
       {:line (dec (:var/name-row semthetic))
        :character (dec (:var/name-end-col semthetic))}})))

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
        (fn [{var-ns :ns
              var-name :name
              var-args :arglists
              var-doc :doc :as m}]
          (let [completion-item-kind {"var" 6
                                      "function" 3
                                      "macro" 3}

                arglists (str/join " " (map #(str "[" % "]") var-args))

                detail (format "%s/%s %s" var-ns var-name arglists)
                detail (if (str/blank? var-doc)
                         detail
                         (format "%s\n\n%s" detail var-doc))]

            {:label var-name
             :kind (completion-item-kind (:type m))
             :detail detail}))))
    (:vars (clojuredocs))))

(def clojuredocs-completion-delay
  (delay (clojuredocs-completion)))


;; ---------------------------------------------------------


(defn analyze
  "Analyze Clojure/Script forms with clj-kondo."
  [{:keys [config lint]}]
  (clj-kondo/run!
    {:lint lint
     :config (or config default-clj-kondo-config)}))

(defn analyze-text
  "Analyze Clojure/Script forms with clj-kondo.

  `uri` is used to report the filename."
  [{:keys [uri text config]}]
  (with-in-str text
    (clj-kondo/run!
      {:lint ["-"]
       :filename (.getPath (URI. uri))
       :config (or config default-clj-kondo-config)})))

(defn analyzer-paths [root-path]
  (let [config-file (io/file root-path "nightincode.edn")]
    (when (.exists config-file)
      (let [config (slurp config-file)
            config (edn/read-string config)
            paths (get-in config [:analyzer :paths])]
        (map #(.getPath (io/file root-path %)) paths)))))

(defn index-V
  "Index Var definitions and usages by symbol and row."
  [analysis]
  (let [index {;; Definitions indexed by name.
               :nightincode/IVD {}

               ;; Definitions indexed by row.
               :nightincode/IVD_ {}

               ;; Usages indexed by name.
               :nightincode/IVU {}

               ;; Usages indexed by row.
               :nightincode/IVU_ {}}

        ;; -- Usage index

        index (reduce
                (fn [index usage]
                  (let [{var-namespace :to
                         var-name :name
                         var-name-row :name-row} usage

                        sym (symbol (str var-namespace) (str var-name))

                        index (update-in index [:nightincode/IVU sym] (fnil conj #{}) usage)

                        index (update-in index [:nightincode/IVU_ var-name-row] (fnil conj []) usage)]

                    index))
                index
                (:var-usages analysis))


        ;; -- Definition index

        index (reduce
                (fn [index definition]
                  (let [{var-namespace :ns
                         var-name :name
                         var-name-row :name-row} definition

                        sym (symbol (str var-namespace) (str var-name))

                        index (update-in index [:nightincode/IVD sym] (fnil conj #{}) definition)

                        index (update-in index [:nightincode/IVD_ var-name-row] (fnil conj []) definition)]

                    index))
                index
                (:var-definitions analysis))]

    index))

(defn index-K
  "Index keyword definitions (reg) and usages by keyword and row."
  [analysis]
  (let [index {;; Definitions indexed by keyword.
               :nightincode/IKD {}

               ;; Definitions indexed by row.
               :nightincode/IKD_ {}

               ;; Usages indexed by keyword.
               :nightincode/IKU {}

               ;; Usages indexed by row.
               :nightincode/IKU_ {}}

        index (reduce
                (fn [index keyword-analysis]
                  (let [{keyword-namespace :ns
                         keyword-name :name
                         keyword-row :row
                         keyword-reg :reg} keyword-analysis

                        k (if keyword-namespace
                            (keyword (str keyword-namespace) keyword-name)
                            (keyword keyword-name))]

                    (cond
                      keyword-reg
                      (-> index
                        (update-in [:nightincode/IKD k] (fnil conj #{}) keyword-analysis)
                        (update-in [:nightincode/IKD_ keyword-row] (fnil conj []) keyword-analysis))

                      :else
                      (-> index
                        (update-in [:nightincode/IKU k] (fnil conj #{}) keyword-analysis)
                        (update-in [:nightincode/IKU_ keyword-row] (fnil conj []) keyword-analysis)))))
                index
                (:keywords analysis))]

    index))


;; -- Indexes

(defn IVD
  "Var definitions indexed by symbol."
  [index]
  (:nightincode/IVD index))

(defn IVD_
  "Var definitions indexed by row.

  Note: row is not zero-based."
  [index]
  (:nightincode/IVD_ index))

(defn IVU
  "Var usages indexed by symbol."
  [index]
  (:nightincode/IVU index))

(defn IVU_
  "Var usages indexed by row.

  Note: row is not zero-based."
  [index]
  (:nightincode/IVU_ index))

(defn IKD
  "Keyword definitions indexed by keyword."
  [index]
  (:nightincode/IKD index))

(defn IKD_
  "Keyword definitions indexed by row.

  Note: row is not zero-based."
  [index]
  (:nightincode/IKD_ index))

(defn IKU
  "Keyword usages indexed by keyword."
  [index]
  (:nightincode/IKU index))

(defn IKU_
  "Keyword usages indexed by row.

  Note: row is not zero-based."
  [index]
  (:nightincode/IKU_ index))


;; -- Queries

(defn ?VD_
  "Returns Var definition at location, or nil."
  [index [row col]]
  (reduce
    (fn [_ {:keys [name-col name-end-col] :as var-definition}]
      (when (<= name-col col name-end-col)
        (reduced var-definition)))
    nil
    ((IVD_ index) row)))

(defn ?VU_
  "Returns Var usage at location, or nil."
  [index [row col]]
  (reduce
    (fn [_ {:keys [name-col name-end-col] :as var-usage}]
      (when (<= name-col col name-end-col)
        (reduced var-usage)))
    nil
    ((IVU_ index) row)))

(defn ?KD_
  "Returns keyword definition at location, or nil."
  [index [row col]]
  (reduce
    (fn [_ {k-col :col
            k-end-col :end-col :as keyword-definition}]
      (when (<= k-col col k-end-col)
        (reduced keyword-definition)))
    nil
    ((IKD_ index) row)))

(defn ?KU_
  "Returns keyword usage at location, or nil."
  [index [row col]]
  (reduce
    (fn [_ {k-col :col
            k-end-col :end-col :as keyword-usage}]
      (when (<= k-col col k-end-col)
        (reduced keyword-usage)))
    nil
    ((IKU_ index) row)))

(defn ?T_
  "Returns T at location, or nil.

  Where T is one of:
   - Namespace definition
   - Namespace usages
   - Var definition
   - Var usage
   - Local definition
   - Local usage
   - Keyword definition
   - Keyword usage"
  [index row+col]
  (reduce
    (fn [_ k]
      (case k
        :nightincode/VD
        (when-let [var-definition (?VD_ index row+col)]
          (reduced (with-meta var-definition {:nightincode/TT :nightincode/VD
                                              :nightincode/row+col row+col})))

        :nightincode/VU
        (when-let [var-usage (?VU_ index row+col)]
          (reduced (with-meta var-usage {:nightincode/TT :nightincode/VU
                                         :nightincode/row+col row+col})))

        :nightincode/KU
        (when-let [keyword-usage (?KU_ index row+col)]
          (reduced (with-meta keyword-usage {:nightincode/TT :nightincode/KU
                                             :nightincode/row+col row+col})))

        :nightincode/KD
        (when-let [keyword-definition (?KD_ index row+col)]
          (reduced (with-meta keyword-definition {:nightincode/TT :nightincode/KD
                                                  :nightincode/row+col row+col})))

        nil))
    nil
    [:nightincode/VU
     :nightincode/VD
     :nightincode/LD
     :nightincode/LU
     :nightincode/KU
     :nightincode/KD]))

(defn TT [T]
  (:nightincode/TT (meta T)))

(defn VD-ident
  "Var definition identity symbol."
  [{:keys [ns name]}]
  (symbol (str ns) (str name)))

(defn VU-ident
  "Var usage identity symbol."
  [{:keys [to name]}]
  (symbol (str to) (str name)))


;; ---------------------------------------------------------

;; -- Functions to read and write from and to state

(def state-ref (atom nil))

(defn _out ^Writer [state]
  (:nightincode/out state))

(defn _repl-port [state]
  (when-let [^ServerSocket server-socket (:nightincode/repl-server-socket state)]
    (.getLocalPort server-socket)))

(defn _root-path [state]
  (get-in state [:LSP/InitializeParams :rootPath]))

(defn _analyzer-conn [state]
  (get-in state [:nightincode/analyzer :conn]))

(defn _project-index [state]
  (get-in state [:nightincode/index :project]))

(defn _text-document-index [state textDocument]
  (get-in state [:nightincode/index (text-document-uri textDocument)]))



(defn !index-document [state {:keys [uri analysis]}]
  (let [index-V (index-V analysis)
        index-K (index-K analysis)

        index (merge index-V index-K)

        state (update-in state [:nightincode/index uri] merge index)]

    state))


;; ---------------------------------------------------------


(defmethod lsp/handle "initialize" [request]

  ;; The initialize request is sent as the first request from the client to the server.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#serverCapabilities

  (let [conn (d/create-conn analyzer/schema)]

    (when-let [paths (analyzer-paths (get-in request [:params :rootPath]))]
      (let [{:keys [analysis]} (analyze {:lint paths})

            index (analyzer/index analysis)

            tx-data (analyzer/prepare-transaction index)]

        (d/transact! conn tx-data)))

    (swap! state-ref assoc
      :LSP/InitializeParams (:params request)
      :nightincode/analyzer {:conn conn}))

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
      :definitionProvider true
      :referencesProvider true
      :documentHighlightProvider true
      :completionProvider {:triggerCharacters ["(" ":"]}
      :documentSymbolProvider true
      :hoverProvider true
      :workspaceSymbolProvider true}

     :serverInfo
     {:name "Nightincode"
      :version "0.3.0-dev"}}))

(defmethod lsp/handle "initialized" [notification]

  ;; The initialized notification is sent from the client to the server after
  ;; the client received the result of the initialize request but before
  ;; the client is sending any other request or notification to the server.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialized

  (let [probe-state (when-let [pid (get-in @state-ref [:LSP/InitializeParams :processId])]

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

    (swap! state-ref merge {:LSP/InitializedParams (:params notification)} probe-state)

    ;; Log a welcome message in the client.
    (lsp/write (_out @state-ref)
      {:jsonrpc "2.0"
       :method "window/logMessage"
       :params {:type 4
                :message (format "Nightincode is up and running!\n\nA REPL is available on port %s.\n\nHappy coding!" (_repl-port @state-ref))}})))

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

(defmethod lsp/handle "$/cancelRequest" [_]
  nil)

(defmethod lsp/handle "textDocument/didOpen" [notification]

  ;; The document open notification is sent from the client to the server to signal newly opened text documents.
  ;; The document’s content is now managed by the client and the server must not try to read the document’s content using the document’s Uri.
  ;; Open in this sense means it is managed by the client. It doesn’t necessarily mean that its content is presented in an editor.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didOpen

  (let [textDocument (get-in notification [:params :textDocument])

        text-document-uri (text-document-uri textDocument)
        text-document-text (text-document-text textDocument)

        result (analyze-text {:uri text-document-uri
                              :text text-document-text})]

    (swap! state-ref !index-document {:uri text-document-uri
                                      :analysis (:analysis result)})))

(defmethod lsp/handle "textDocument/didChange" [notification]

  ;; The document change notification is sent from the client to the server to signal changes to a text document.
  ;; Before a client can change a text document it must claim ownership of its content using the textDocument/didOpen notification.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didChange

  (let [textDocument (get-in notification [:params :textDocument])

        text-document-uri (text-document-uri textDocument)

        ;; The client sends the full text because textDocumentSync capability is set to 1 (full).
        text-document-text (get-in notification [:params :contentChanges 0 :text])

        result (analyze-text {:uri text-document-uri
                              :text text-document-text})]

    (swap! state-ref !index-document {:uri text-document-uri
                                      :analysis (:analysis result)})))

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

  (let [textDocument (get-in notification [:params :textDocument])]
    (swap! state-ref
      (fn [state]
        (let [text-document-uri (text-document-uri textDocument)

              state (update state :nightincode/index dissoc text-document-uri)
              state (update state :clj-kondo/result dissoc text-document-uri)]

          state)))))

(defmethod lsp/handle "textDocument/definition" [request]

  ;; The go to definition request is sent from the client to the server
  ;; to resolve the definition location of a symbol at a given text document position.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_definition

  (try
    (let [textDocument (get-in request [:params :textDocument])

          cursor-line (get-in request [:params :position :line])
          cursor-character (get-in request [:params :position :character])

          db (d/db (_analyzer-conn @state-ref))

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
                      (analyzer/?definitions db cursor-semthetic))]

      (lsp/response request (seq locations)))

    (catch Exception ex
      (lsp/error-response request
        {:code -32803
         :message (format "Nightincode failed to find definitions. (%s)\n"
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

          db (d/db (_analyzer-conn @state-ref))

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
                      (analyzer/?usages db cursor-semthetic))]

      (lsp/response request (seq locations)))

    (catch Exception ex
      (lsp/error-response request
        {:code -32803
         :message (format "Nightincode failed to find references. (%s)\n"
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

          db (d/db (_analyzer-conn @state-ref))

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
                                   (analyzer/?file-definitions db cursor-semthetic))

          usages-highlights (mapcat
                              (fn [{:semthetic/keys [locs]}]
                                (map
                                  (fn [loc]
                                    {:range (analyzer/loc-range loc)})
                                  locs))
                              (analyzer/?file-usages db cursor-semthetic))

          highlights (reduce into [] [definitions-highlights
                                      usages-highlights])]

      (lsp/response request (seq highlights)))

    (catch Exception ex
      (lsp/error-response request
        {:code -32803
         :message (format "Nightincode failed to find highlights. (%s)\n"
                    (with-out-str (stacktrace/print-stack-trace ex)))}))))

(defmethod lsp/handle "textDocument/completion" [request]

  ;; The Completion request is sent from the client to the server to compute completion items at a given cursor position.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion

  (try
    (let [textDocument (get-in request [:params :textDocument])

          cursor-line (get-in request [:params :position :line])

          index (_text-document-index @state-ref textDocument)

          row+col [(inc (get-in request [:params :position :line]))
                   (inc (get-in request [:params :position :character]))]

          T (?T_ index row+col)

          ;; TODO: Extract completions.

          ;; Completions with document definitions.
          VD-completions (into []
                           (map
                             (fn [[sym _]]
                               ;; Var name only because it's a document definition.
                               {:label (name sym)
                                :kind 6}))
                           (IVD index))

          ;; Completions with document usages - exclude "self definitions".
          VU-completions (into #{}
                           (comp
                             (mapcat val)
                             (remove
                               (fn [{:keys [from to]}]
                                 (= from to)))
                             (map
                               (fn [{:keys [to alias name]}]
                                 {:label (cond
                                           (contains? #{'clojure.core 'cljs.core} to)
                                           (str name)

                                           (or alias to)
                                           (format "%s/%s" (or alias to) name)

                                           :else
                                           (str name))
                                  :kind 6})))
                           (IVU index))

          K-completions (into []
                          (map
                            (fn [[k _]]
                              {:label (str k)
                               :kind 14}))
                          ;; Only the keyword is necessary,
                          ;; so it's okay to combine indexes.
                          (merge (IKD index) (IKU index)))

          completions (reduce
                        into
                        []
                        [VD-completions
                         VU-completions
                         K-completions])]

      (lsp/response request (merge {:items completions}
                              (when T
                                {:itemDefaults
                                 {:editRange
                                  {:start
                                   {:line cursor-line
                                    :character (dec (or (:name-col T) (:col T)))}
                                   :end
                                   {:line cursor-line
                                    :character (dec (or (:name-end-col T) (:end-col T)))}}}}))))
    (catch Exception ex
      (lsp/error-response request
        {:code -32803
         :message (format "Nightincode failed to compute completions. (%s)"(ex-message ex))}))))

(defmethod lsp/handle "textDocument/documentSymbol" [request]

  ;; The document symbol request is sent from the client to the server.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_documentSymbol

  (try
    (let [textDocument (get-in request [:params :textDocument])

          db (d/db (_analyzer-conn @state-ref))

          ;; Find namespace and var definitions in file.
          definitions (d/q '[:find [(pull ?e [*]) ...]
                             :in $ ?filename
                             :where
                             [?e :semthetic/filename ?filename]
                             [?e :semthetic/semantic :def]
                             (or
                               [?e :semthetic/modifier :namespace]
                               [?e :semthetic/modifier :var])]
                        db (text-document-path textDocument))

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

          db (d/db (_analyzer-conn @state-ref))

          cursor-semthetic (analyzer/?cursor-semthetic db
                             {:filename (text-document-path textDocument)
                              :row (inc cursor-line)
                              :col (inc cursor-character)
                              :col-end (inc cursor-character)})

          cursor-definitions (analyzer/?definitions db cursor-semthetic)]

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
    (let [db (d/db (_analyzer-conn @state-ref))

          definitions (d/q '[:find [(pull ?e [*]) ...]
                             :where
                             [?e :semthetic/semantic :def]
                             (or
                               [?e :semthetic/modifier :namespace]
                               [?e :semthetic/modifier :var])]
                        db)

          symbols (mapcat
                    (fn [{:semthetic/keys [modifier filename locs] :as semthetic}]
                      (map
                        (fn [loc]
                          {:name (or (semthetic-label semthetic) "?")
                           :kind (semthetic-symbol-kind semthetic)
                           :location (analyzer/loc-location filename loc)})
                        locs))
                    definitions)]

      (lsp/response request (seq symbols)))

    (catch Exception ex
      (lsp/error-response request
        {:code -32803
         :message (format "Nightincode failed to find workspace symbols. (%s)\n"
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
     :out (lsp/buffered-writer System/out)
     :trace
     (fn [{:keys [header status content error] :as trace}]
       (case status
         :read
         (log/debug status (select-keys trace [:header :numbytes]))

         ;; Note: `content` is a string when there's a decoding error.
         :decode-error
         (log/error error status header content)

         :handled
         (log/debug status (select-keys content [:id :method]))

         nil))}))


(comment

  (keys @state-ref)

  (let [{:keys [LSP/InitializeParams
                LSP/InitializedParams

                nightincode/index
                clj-kondo/result]} @state-ref]

    (def initialize-params InitializeParams)
    (def initialized-params InitializedParams)
    (def index index)
    (def clj-kondo-result result))

  (keys index)

  (:project index)


  ;; -- Queries

  (def conn (_analyzer-conn @state-ref))

  ;; Every Namespace:
  (d/q '[:find  [(pull ?v [:namespace/name]) ...]
         :where
         [?v :namespace/filename]]
    (d/db conn))

  ;; Every Namespace usage:
  (d/q '[:find  [(pull ?v [:namespace-usage/to :namespace-usage/lang]) ...]
         :where
         [?v :namespace-usage/filename]]
    (d/db conn))

  ;; Every Var:
  (d/q '[:find  [(pull ?v [:var/ns :var/name]) ...]
         :where
         [?v :var/filename]]
    (d/db conn))

  ;; Every Var usage:
  (d/q '[:find  [(pull ?v [:var-usage/from :var-usage/to :var-usage/name]) ...]
         :where
         [?v :var-usage/filename]]
    (d/db conn))

  ;; Var usage missing name row & rol:
  (d/q '[:find  [(pull ?v [*]) ...]
         :where
         [?v :var-usage/filename]
         [(missing? $ ?v :var-usage/name-row)]]
    (d/db conn))

  ;; Every local:
  (d/q '[:find  [(pull ?v [*]) ...]
         :where
         [?v :local/filename]]
    (d/db conn))

  ;; Every local usage:
  (d/q '[:find  [(pull ?v [*]) ...]
         :where
         [?v :local-usage/filename]]
    (d/db conn))

  ;; Local usage missing name row & rol:
  (d/q '[:find  [(pull ?v [*]) ...]
         :where
         [?v :local-usage/filename]
         [(missing? $ ?v :local-usage/name-row)]]
    (d/db conn))

  ;; Every definition:
  (d/q '[:find  [(pull ?e [*]) ...]
         :where
         [?e :semthetic/semantic :def]]
    (d/db conn))

  ;; Every usage:
  (d/q '[:find  [(pull ?e [*]) ...]
         :where
         [?e :semthetic/semantic :usage]]
    (d/db conn))


  (d/q '[:find  [(pull ?f [*]) ...]
         :in $ ?path
         :where
         [?f :file/path ?path]]
    (d/db (_analyzer-conn @state-ref))
    "/Users/pedro/Developer/lispi/src/lispi/core.clj")

  (d/q '[:find  [(pull ?e [*]) ...]
         :where
         [?e :loc/locs ?locs]
         [?locs :loc/row 189]
         [?locs :loc/col ?col]
         [?locs :loc/col-end ?col-end]
         [(>= ?col 12)]
         [(<= ?col-end 12)]]
    (d/db conn))

  (d/q '[:find  [(pull ?e [*]) ...]
         :where
         [?e :semthetic/locs ?locs]
         [?e :semthetic/filename "/Users/pedro/Developer/Nightincode/src/nightincode/analyzer.clj"]
         [?locs :loc/row 183]
         [?locs :loc/col ?col]
         [?locs :loc/col-end ?col-end]
         [(>= 7 ?col)]
         [(<= 12 ?col-end)]]
    (d/db conn))

  (d/q '[:find  [(pull ?e [*]) ...]
         :where
         [?e :semthetic/locs ?locs]
         [?e :semthetic/filename "/Users/pedro/Developer/Nightincode/src/nightincode/analyzer.clj"]
         [?locs :loc/row 183]]
    (d/db conn))

  (analyzer/?cursor-semthetic (d/db conn)
    {:filename "/Users/pedro/Developer/Nightincode/src/nightincode/analyzer.clj"
     :row 216
     :col 15
     :col-end 15})

  (analyzer/?cursor-semthetic (d/db conn)
    {:filename "/Users/pedro/Developer/Nightincode/src/nightincode/server.clj"
     :row 1032
     :col 26
     :col-end 26})

  (analyzer/?definitions (d/db conn) *1)

  (analyzer/?usages (d/db conn)
    '{:semthetic/semantic :def,
      :semthetic/modifier :var,
      :semthetic/identifier nightincode.analyzer/?semthetic_})

  (d/q '[:find [(pull ?e [*]) ...]
         :in $ ?qualifier ?identifier
         :where
         [?e :semthetic/semantic :usage]
         ]
    (d/db conn) :var 'nightincode.analyzer/?semthetic_)

  (analyzer/?cursor-semthetic (d/db conn)
    {:filename "/Users/pedro/Developer/Nightincode/src/nightincode/analyzer.clj"
     :row 183
     :col 31
     :col-end 31})

  (d/q '[:find  ?l
         :in $
         :where
         [?l :loc/locs]]
    (d/db (_analyzer-conn @state-ref)))

  (lsp/handle
    {:method "textDocument/completion"
     :params
     {:textDocument
      {:uri "file:///Users/pedro/Developer/lispi/src/lispi/core.clj"}

      :position
      {:line 119
       :character 40}}})

  (def lispi-core-uri "file:///Users/pedro/Developer/lispi/src/lispi/core.clj")

  (clj-kondo/run!
    {:lint [lispi-core-uri]
     :config default-clj-kondo-config})

  '{:row 42,
    :col 29,
    :end-row 42,
    :end-col 32,
    :name "as",
    :filename "/Users/pedro/Developer/lispi/src/lispi/core.clj",
    :from user}

  '{:end-row 49,
    :ns lispi,
    :name "tokens",
    :filename "/Users/pedro/Developer/lispi/src/lispi/core.clj",
    :from lispi.core,
    :col 8,
    :reg clojure.spec.alpha/def,
    :end-col 21,
    :row 49}

  (IVD_ (_text-document-index @state-ref {:uri lispi-core-uri}))
  (?VD_ (_text-document-index @state-ref {:uri lispi-core-uri}) [112 13])

  (IVU_ (_text-document-index @state-ref {:uri lispi-core-uri}))
  (?VU_ (_text-document-index @state-ref {:uri lispi-core-uri}) [184 15])

  (IVD (_text-document-index @state-ref {:uri lispi-core-uri}))
  (IVU (_text-document-index @state-ref {:uri lispi-core-uri}))

  (meta (?T_ (_text-document-index @state-ref {:uri lispi-core-uri}) [112 13]))

  (keys index)

  (lsp/write (_out @state-ref)
    {:jsonrpc "2.0"
     :method "window/showMessage"
     :params {:type 3
              :message "Hello!"}})

  (require '[clojure.tools.cli.api :as deps-cli])
  (require '[clojure.tools.deps.alpha :as deps])

  (def deps-file
    (deps/slurp-deps (io/file "/Users/pedro/Developer/Nightincode/deps.edn")))

  (deps/calc-basis deps-file)

  (deps-cli/list
    {:project deps-file
     :license :none})


  )
