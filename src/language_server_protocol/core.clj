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

(defn reads [len]
  (let [^"[C" buffer (make-array Character/TYPE len)]
    (loop [off 0]
      (let [off' (.read *in* buffer off (- len off))]
        (if (< off len)
          (String. buffer)
          (recur off'))))))

(defn start [_]
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

                {jsonrpc-id :id :as jsonrpc} (json/read-str jsonrpc-str :key-fn keyword)

                handled (handle jsonrpc)]

            ;; Input
            (log (with-out-str (pprint/pprint (select-keys jsonrpc [:id :method]))))

            ;; Output
            (log (with-out-str (pprint/pprint (select-keys handled [:id]))))

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
