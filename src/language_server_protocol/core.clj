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

(defn response [request]
  (let [{:keys [id]} request]
    {:id id
     :jsonrpc "2.0"
     :result
     {:capabilities
      {:hoverProvider true}

      :serverInfo
      {:name "Nightincode"}}}))

(defn parse-header [chars]
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

(defn -main [& _]
  (let [parser-state-ref (atom {:eof? false
                                :chars []
                                :newline# 0})]
    (while (not (:eof?  @parser-state-ref))
      (let [{:keys [chars newline#]} @parser-state-ref]
        (cond
          ;; Two consecutive newline characters - parse header and content.
          (= newline# 2)
          (let [{:keys [Content-Length] :as header} (parse-header chars)

                ;; Increment len to account for \newline before content.
                ;; Note: I don't quite understand why `reads` is consuming the \newline - I need to look into it.
                jsonrpc-str (reads (inc Content-Length))
                jsonrpc (json/read-str jsonrpc-str :key-fn keyword)

                r {:id 0
                   :jsonrpc "2.0"
                   :result
                   {:capabilities
                    {:hoverProvider true}

                    :serverInfo
                    {:name "Nightincode"}}}

                r (json/write-str r)
                r (format "Content-Length: %s\r\n\r\n%s" (alength (.getBytes r)) r)]

            (log (with-out-str (pprint/pprint jsonrpc)))

            (print r)

            (flush)

            (reset! parser-state-ref {:chars []
                                      :newline# 0}))

          :else
          (let [c (.read *in*)]
            (reset! parser-state-ref {:eof? (= c -1)
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


  (with-in-str "Foo1: 1\r\nFoo2: 1\r\n"
    (.readLine *in*))

  (with-in-str "\r\n\r\n"
    (.readLine *in*))


  (with-in-str (json/write-str {:a 1
                                :b 2
                                :c 3})
    (json/read *in* :key-fn keyword))



  (let [line-ref (atom nil)

        header-ref (atom [])]

    (with-open [r (LineNumberingPushbackReader.
                    (StringReader. (str "Foo1: 1\r\nFoo2: 2\r\n\r\n" (json/write-str {:a 1
                                                                                      :b 2
                                                                                      :c 3}))))]

      (while (reset! line-ref (.readLine r))
        (let [s @line-ref]
          (cond
            (str/blank? s)
            (prn @header-ref (json/read r))

            :else
            (swap! header-ref conj s))))))

  (let [char-ref (atom nil)

        parser-state-ref (atom {:chars []
                                :newline# 0})]

    (with-open [r (LineNumberingPushbackReader.
                    (StringReader. (str "Content-Length: 1\r\nHeader 2: 2\r\n\r\n"
                                     (json/write-str {:a 1
                                                      :b 2
                                                      :c 3}))))]

      (while (not= (reset! char-ref (.read r)) -1)

        (let [{:keys [chars newline#]} @parser-state-ref

              char (char @char-ref)

              newline? (= (char-name-string char) "newline")

              chars (conj chars char)]

          (cond
            ;; Two consecutive newline characters - parse header and content.
            (and newline? (= newline# 1))
            (let [header (str/split-lines (apply str chars))
                  header (->> header
                           (map
                             (fn [line]
                               (let [[k v] (str/split line #":")

                                     v (cond
                                         (= (str/lower-case k) "content-length")
                                         (parse-long (str/trim v))

                                         :else
                                         v)]

                                 {k v})))
                           (into {}))]

              (prn header)

              (reset! parser-state-ref {:chars []
                                        :newline# 0}))

            :else
            (reset! parser-state-ref {:chars chars
                                      ;; Reset newline counter when next character is part of the header.
                                      :newline# (if newline?
                                                  (inc newline#)
                                                  0)})))))

    @parser-state-ref)


  (parse-header (char-array "Content-Length: 1\r\nHeader 2: 2\r\n\r\n"))
  ;; =>
  {:Content-Length 1, "Header 2" " 2"}



  )
