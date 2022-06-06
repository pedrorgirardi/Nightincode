(ns language-server-protocol.core
  (:require
   [clojure.string :as str]
   [clojure.data.json :as json]
   [clojure.pprint :as pprint])

  (:import
   (java.io
    File
    StringReader)

   (clojure.lang
    LineNumberingPushbackReader))

  (:gen-class))

(defn log [& s]
  (let [f (File. (System/getProperty "java.io.tmpdir") "Nightincode.log")]
    (spit f (str (str/join " " s) "\n") :append true)))

(defn response [jsonrpc result]
  (let [{:keys [id]} jsonrpc]
    {:id id
     :jsonrpc "2.0"
     :result result}))

(defn header [chars]
  (->> (str/split-lines (apply str chars))
    (map
      (fn [line]
        (let [[k v] (str/split line #":")]
          (cond
            (= (str/lower-case k) "content-length")
            [:Content-Length (parse-long (str/trim v))]

            :else
            [k v]))))
    (into {})))

(defn reads [len]
  (let [^"[C" buffer (make-array Character/TYPE len)]
    (loop [off 0]
      (let [off' (.read *in* buffer off (- len off))]
        (if (< off len)
          (String. buffer)
          (recur off'))))))

(def default-implementation
  {;; The initialize request is sent as the first request from the client to the server.
   ;;
   ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
   "initialize" (fn [jsonrpc]
                  (response jsonrpc {:capabilities
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
                                      :completionProvider {}}}))

   ;; The hover request is sent from the client to the server to request hover information at a given text document position.
   ;;
   ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_hover
   "textDocument/hover" (fn [jsonrpc]
                          (response jsonrpc nil))

   ;; The Completion request is sent from the client to the server to compute completion items at a given cursor position.
   ;;
   ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion
   "textDocument/completion" (fn [jsonrpc]
                               (response jsonrpc nil))})

(defn start [{:keys [method->handler]}]
  (let [state-ref (atom {:eof? false
                         :chars []
                         :newline# 0})]
    (while (not (:eof?  @state-ref))
      (let [{:keys [chars newline#]} @state-ref]
        (cond
          ;; Two consecutive newline characters - parse header and content.
          (= newline# 2)
          (let [{:keys [Content-Length]} (header chars)

                ;; Increment len to account for \newline before content.
                ;; Note: I don't quite understand why `reads` is consuming the \newline - I need to look into it.
                jsonrpc-str (reads (inc Content-Length))

                {jsonrpc-id :id
                 jsonrpc-method :method :as jsonrpc} (json/read-str jsonrpc-str :key-fn keyword)

                handler (or (method->handler jsonrpc-method) (constantly nil))

                handled (handler jsonrpc)]

            ;; Input
            (log (with-out-str (pprint/pprint (select-keys jsonrpc [:id :method]))))

            ;; Output
            (log (with-out-str (pprint/pprint handled)))

            ;; Don't send a response back for a notification.
            ;; (It's assumed that only requests have ID.)
            ;;
            ;; > Every processed request must send a response back to the sender of the request.
            ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#requestMessage
            ;;
            ;; > A processed notification message must not send a response back. They work like events.
            ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#notificationMessage
            (when jsonrpc-id
              (let [response-str (json/write-str handled)
                    response-str (format "Content-Length: %s\r\n\r\n%s" (alength (.getBytes response-str)) response-str)]

                (print response-str)

                (flush)))

            (reset! state-ref {:chars []
                               :newline# 0}))

          :else
          (let [c (.read *in*)]
            (reset! state-ref {:eof? (= c -1)
                               :chars (conj chars (char c))
                               ;; Reset newline counter when next character is part of the header.
                               :newline# (if (= (char c) \newline)
                                           (inc newline#)
                                           0)})))))))

(comment

  (def r
    (LineNumberingPushbackReader.
      (StringReader. "Foo1: 1\r\nFoo2: 2\r\n\r\n{}")))

  (char (.read r))


  )
