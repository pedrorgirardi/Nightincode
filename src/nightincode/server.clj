(ns nightincode.server
  (:require
   [clojure.core.server :refer [start-server]]
   [clojure.java.io :as io]
   [clojure.data.json :as json]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [clojure.java.shell :as shell]

   [clj-kondo.core :as clj-kondo]
   [lspie.api :as lsp])

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
  "Analyze Clojure/Script forms with clj-kondo.

  `uri` is used to report the filename."
  [{:keys [uri text config]}]
  (with-in-str text
    (clj-kondo/run!
      {:lint ["-"]
       :filename (.getPath (URI. uri))
       :config (or config default-clj-kondo-config)})))

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


(defn V-location
  "Returns a LSP Location for a Var definition or usage.

  https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#location"
  [{:keys [filename
           name-row
           name-end-row
           name-col
           name-end-col]}]
  {:uri (.toString (filepath-uri filename))
   :range
   {:start
    {:line (dec name-row)
     :character (dec name-col)}
    :end
    {:line (dec name-end-row)
     :character (dec name-end-col)}}})

;; ---------------------------------------------------------

;; -- Functions to read and write from and to state

(def state-ref (atom nil))

(defn _out ^Writer [state]
  (:nightincode/out state))

(defn _repl-port [state]
  (when-let [^ServerSocket server-socket (:nightincode/repl-server-socket state)]
    (.getLocalPort server-socket)))

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
      :definitionProvider true
      :referencesProvider true
      :completionProvider {:triggerCharacters ["(" ":"]}}

     :serverInfo
     {:name "Nightincode"}}))

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

(defmethod lsp/handle "textDocument/didOpen" [notification]

  ;; The document open notification is sent from the client to the server to signal newly opened text documents.
  ;; The document’s content is now managed by the client and the server must not try to read the document’s content using the document’s Uri.
  ;; Open in this sense means it is managed by the client. It doesn’t necessarily mean that its content is presented in an editor.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didOpen

  (let [textDocument (get-in notification [:params :textDocument])

        text-document-uri (text-document-uri textDocument)
        text-document-text (text-document-text textDocument)

        result (analyze {:uri text-document-uri
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

        result (analyze {:uri text-document-uri
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

          index (_text-document-index @state-ref textDocument)

          row+col [(inc cursor-line) (inc cursor-character)]

          T (?T_ index row+col)

          D (case (TT T)
              :nightincode/VD
              (map V-location ((IVD index) (VD-ident T)))

              :nightincode/VU
              (map V-location ((IVD index) (VU-ident T)))

              nil)]

      (lsp/response request (seq D)))

    (catch Exception ex
      (lsp/error-response request
        {:code -32803
         :message (format "Sorry. Nightincode failed to find a definition. (%s)"(ex-message ex))}))))

(defmethod lsp/handle "textDocument/references" [request]

  ;; The references request is sent from the client to the server
  ;; to resolve project-wide references for the symbol denoted by the given text document position.
  ;;
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_references

  (try
    (let [textDocument (get-in request [:params :textDocument])

          cursor-line (get-in request [:params :position :line])
          cursor-character (get-in request [:params :position :character])

          index (_text-document-index @state-ref textDocument)

          row+col [(inc cursor-line) (inc cursor-character)]

          T (?T_ index row+col)

          R (case (TT T)
              :nightincode/VD
              (map V-location ((IVU index) (VD-ident T)))

              :nightincode/VU
              (map V-location ((IVU index) (VU-ident T)))

              nil)]

      (lsp/response request (seq R)))

    (catch Exception ex
      (lsp/error-response request
        {:code -32803
         :message (format "Sorry. Nightincode failed to find references. (%s)"(ex-message ex))}))))

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
         :message (format "Sorry. Nightincode failed to compute completions. (%s)"(ex-message ex))}))))

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


  )
