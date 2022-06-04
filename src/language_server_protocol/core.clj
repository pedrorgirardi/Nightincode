(ns language-server-protocol.core
  (:require
   [clojure.string :as str]
   [clojure.data.json :as json])

  (:import
   (java.io
    File
    StringReader)

   (java.util.concurrent
    CompletableFuture)

   (org.eclipse.lsp4j.jsonrpc
    Endpoint)

   (clojure.lang
    LineNumberingPushbackReader))

  (:gen-class))

(defn log [& s]
  (let [f (File. (System/getProperty "java.io.tmpdir") "Nightincode.log")]
    (spit f (str "\nDEBUG " (str/join " " s)) :append true)))

(defn parse-content-header-string [s]
  ;; Examples:
  ;;
  ;; (parse-content-header-string "{}Content-Length: 183")
  ;; => {:header "Content-Length: 183", :content "{}"}
  ;;
  ;; (parse-content-header-string "{}")
  ;; => {:header "", :content "{}"}
  ;;
  ;; (parse-content-header-string "{}   ")
  ;; => {:header "", :content "{}"}
  ;;
  ;; (parse-content-header-string "{}       Content-Length: 183")
  ;; => {:header "Content-Length: 183", :content "{}"}

  ;; Search backwards for the last '}'.
  (let [last-curly-index (str/last-index-of s "}" (dec (count s)))]
    {:header (str/triml (subs s (inc last-curly-index)))
     :content (subs s 0 (inc last-curly-index))}))

(defn response [request]
  (let [{:keys [id]} request]
    {:id id
     :jsonrpc "2.0"
     :result
     {:capabilities
      {:hoverProvider true}

      :serverInfo
      {:name "Nightincode"}}}))

(defn -main [& _]
  (let [char-ref (atom nil)

        process-ref (atom {:newline 0
                           :header []
                           :parse-next? false})]

    (while (not= (reset! char-ref (.read *in*)) -1)
      (cond
        ;; Bookkepping of newline - so we know when it's ready to read content.
        (= (char-name-string @char-ref) "newline")
        (swap! process-ref update :newline inc)

        ;; Ready to process content (JSON).
        (= (:newline @process-ref) 2)
        nil

        ;; Construct header one character at a time.
        :else
        (swap! process-ref update :header conj (char @char-ref))))))


#_(defn -main [& _]
    (let [counter-ref (atom 0)

          line-ref (atom nil)]

      (while (not= (reset! line-ref (.read *in*)) -1)

        (swap! counter-ref inc)

        (log (or (char-name-string (char @line-ref)) (char @line-ref)))

        (let [r {:id 0
                 :jsonrpc "2.0"
                 :result
                 {:capabilities
                  {:hoverProvider true}

                  :serverInfo
                  {:name "Nightincode"}}}

              r (json/write-str r)

              r (format "Content-Length: %s\r\n\r\n%s" (alength (.getBytes r)) r)]

          (print r)

          (flush)))))


(comment

  (parse-content-header-string "{}Content-Length: 183")
  ;; => {:header "Content-Length: 183", :content "{}"}

  (parse-content-header-string "{}")
  ;; => {:header "", :content "{}"}

  (parse-content-header-string "{}   ")
  ;; => {:header "", :content "{}"}

  (parse-content-header-string "{}       Content-Length: 183")
  ;; => {:header "Content-Length: 183", :content "{}"}


  (def r
    (LineNumberingPushbackReader.
      (StringReader. "Foo1: 1\r\nFoo2: 2\r\n\r\n{}")))

  (.readLine r)

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
                    (StringReader. (str "Header: 1\r\nHeader 2: 2\r\n\r\n" (json/write-str {:a 1
                                                                                            :b 2
                                                                                            :c 3}))))]

      (while (not= (reset! char-ref (.read r)) -1)

        (swap! parser-state-ref
          (fn [{:keys [chars newline#]}]
            (let [char (char @char-ref)

                  newline? (= (char-name-string char) "newline")

                  chars (conj chars char)]

              (cond
                ;; Two consecutive newline characters.
                (and newline? (= newline# 1))
                (let [header (str/split-lines (apply str chars))
                      header (->> header
                               (map
                                 (fn [line]
                                   (let [[k v] (str/split line #":")]
                                     {k v})))
                               (into {}))]
                  (prn header)

                  {:chars []
                   :newline# 0})

                :else
                {:chars chars
                 :newline# (if newline?
                             (inc newline#)
                             newline#)}))))))

    @parser-state-ref)


  ;; An endpoint is a generic interface that accepts jsonrpc requests and notifications.
  (def endpoint
    (reify Endpoint
      (^void notify [_ method parameter]
       (tap> ['notify method parameter]))

      (^CompletableFuture request [_ method parameter]
       (tap> ['request method parameter])

       (CompletableFuture/completedFuture
         (let [[_ jsonrpc] (str/split (slurp *in*) #"\r\n\r\n")

               {:strs [id]} (json/read-str jsonrpc)

               result (json/write-str {:id id
                                       :jsonrpc "2.0"
                                       :result
                                       {:capabilities
                                        {:hoverProvider true}

                                        :serverInfo
                                        {:name "Nightincode"}}})]

           (format "Content-Length: %s\r\n\r\n%s" (alength (.getBytes result)) result))))))


  )
