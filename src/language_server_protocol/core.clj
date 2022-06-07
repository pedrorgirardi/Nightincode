(ns language-server-protocol.core
  (:require
   [clojure.string :as str]
   [clojure.data.json :as json])

  (:import
   (java.io
    File
    Reader

    InputStreamReader
    OutputStreamWriter

    BufferedReader
    BufferedWriter))

  (:gen-class))

(defn response [request result]
  (merge (select-keys request [:id :jsonrpc]) {:result result}))


;; --------------------------------


(defmulti handle :method)

(defmethod handle :default [jsonrpc]
  (response jsonrpc nil))


;; --------------------------------


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

(defn reads [^Reader reader len]
  (let [^"[C" buffer (make-array Character/TYPE len)]
    (loop [off 0]
      (let [off' (.read reader buffer off (- len off))]
        (if (< off len)
          (String. buffer)
          (recur off'))))))

(defn start [{:keys [in out trace]}]
  (let [^BufferedReader reader (BufferedReader. (InputStreamReader. in "UTF-8"))

        ^BufferedWriter writer (BufferedWriter. (OutputStreamWriter. out "UTF-8"))

        trace (or trace identity)

        initial-state {:chars []
                       :newline# 0
                       :return# 0}]

    (loop [{:keys [chars newline# return#] :as state} initial-state]
      (cond
        ;; Two consecutive return & newline characters - parse header and content.
        (and (= return# 2) (= newline# 2))
        (let [{:keys [Content-Length] :as header} (header chars)

              jsonrpc-str (reads reader Content-Length)

              ;; Let the client know that the message, request or notification, was decoded.
              trace-decoded (fn [jsonrpc]
                              (trace {:status :message-decoded
                                      :header header
                                      :content jsonrpc}))

              {jsonrpc-id :id :as jsonrpc} (doto (json/read-str jsonrpc-str :key-fn keyword) trace-decoded)

              ;; Let the client know that the message, request or notification, was handled.
              trace-handled (fn [handled]
                              (trace {:status :message-handled
                                      :header header
                                      :content jsonrpc
                                      :handled handled}))

              handled (doto (handle jsonrpc) trace-handled)]

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

              (.write writer response-str)
              (.flush writer)

              ;; Let the client know that a response was sent for the request.
              (trace {:status :response-sent
                      :response response-str})))

          (recur initial-state))

        :else
        (let [c (.read reader)]
          (when-not (= c -1)
            (recur (merge state {:chars (conj chars (char c))}
                     (cond
                       (= (char c) \newline)
                       {:newline# (inc newline#)}

                       (= (char c) \return)
                       {:return# (inc return#)}

                       ;; Reset return & newline counter when next character is part of the header.
                       :else
                       {:return# 0
                        :newline# 0})))))))))

(comment


  (defn rpc
    [^java.io.Reader in ^java.io.Writer out]
    (loop []
      (let [recur? (try
                     (let [header (.readLine in)
                           ;; TODO: Actually parse header
                           len (parse-long (str/trim (second (str/split header #":"))))]
                       ;; Discard empty line
                       (.readLine in)
                       (let [buf (make-array Character/TYPE len)
                             chars-read (.read in buf 0 len)]
                         (if (neg? chars-read)
                           :eof
                           (let [message {:header header :content (json/read-str (String. buf))}]
                             (tap> [:actually-handle-message message])
                             (.write out (format "Handled message %s\n" (pr-str message)))
                             (.flush out)
                             true))))
                     (catch java.io.IOException _
                       ;; Stream closed
                       (tap> :exit)
                       false))]
        (when recur? (recur)))))

  (import '(java.io BufferedReader BufferedWriter PipedReader PipedWriter))

  (with-open [pipe-in (PipedWriter.)
              in-writer (BufferedWriter. pipe-in)
              in (BufferedReader. (PipedReader. pipe-in))

              pipe-out (PipedReader.)
              out-reader (BufferedReader. pipe-out)
              out (BufferedWriter. (PipedWriter. pipe-out))]
    (let [f (future (rpc in out))
          message (json/write-str {"jsonrpc" "2.0" "id" 1 "method" "textDocument/didOpen" "params" {}})]
      (try
        ;; `count` is probably the wrong way to calculate the length of the message?
        (.write in-writer (format "Content-Length: %d\r\n\r\n" (count message)))
        (.write in-writer message)
        (.flush in-writer)
        (tap> (.readLine out-reader))
        (catch Throwable _
          (future-cancel f)))))


  )
