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
    (spit f (apply str "\nDEBUG " s) :append true)))

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
  (let [line-ref (atom nil)

        process-ref (atom {})

        header-ref (atom [])]

    (while (reset! line-ref (.readLine *in*))
      (let [line @line-ref]
        (cond
          (str/blank? line)
          (do
            (log "BLANK/PARSE NEXT" line)
            (swap! process-ref assoc :parse-next? true)
            (print "")
            (flush))

          (:parse-next? @process-ref)
          (let [_ (log "PARSE" line)

                {:keys [content]} (parse-content-header-string line)

                jsonrpc (json/read-str content)

                r (response jsonrpc)
                r (json/write-str r)
                r (format "Content-Length: %s\r\n\r\n%s" (alength (.getBytes r)) r)]

            (print r)
            (flush))

          :else
          (do
            (log "HEADER: " line)
            (swap! header-ref conj line)))))))


#_(defn -main [& _]
  (let [counter-ref (atom 0)

        line-ref (atom nil)]

    (while (reset! line-ref (.readLine *in*))

      (swap! counter-ref inc)

      (log (str "### " @counter-ref " ### ") @line-ref)

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

  (let [line-ref (atom nil)

        process-ref (atom {:header []
                           :blank? false})]


    (with-open [r (LineNumberingPushbackReader.
                    (StringReader. (str "Foo1: 1\r\nFoo2: 2\r\n\r\n" (json/write-str {:a 1
                                                                                      :b 2
                                                                                      :c 3}))))]

      (while (reset! line-ref (.readLine r))
        (cond
          (:blank? @process-ref)
          (prn (:header @process-ref) (json/read-str @line-ref))

          (str/blank? @line-ref)
          (swap! process-ref assoc :blank? true)

          :else
          (swap! process-ref update :header conj @line-ref)))))


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
