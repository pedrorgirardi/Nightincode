(ns language-server-protocol.core
  (:require
   [clojure.string :as str]
   [clojure.data.json :as json])

  (:import
   (java.io
    StringReader)

   (java.util.concurrent
    CompletableFuture)

   (org.eclipse.lsp4j.jsonrpc
    Endpoint)

   (clojure.lang
    LineNumberingPushbackReader))

  (:gen-class))

(defn response [request]
  (let [{:keys [id]} request]
    {:id id
     :jsonrpc "2.0"
     :result
     {:capabilities
      {:hoverProvider true}

      :serverInfo
      {:name "Nightincode"}}}))

(defn log [s]
  (spit "Nightincode.log" s :append true))

(defn -main [& _]
  (let [line-ref (atom nil)

        header-ref (atom [])]

    (while (reset! line-ref (.readLine *in*))

      (log @line-ref)

      (cond
        (str/blank? @line-ref)
        (let [_ (log "Read after blank")

              s (.readLine *in*)

              _ (log (str "After blank - before JSON " s))

              jsonrpc (json/read-str s)

              r (response jsonrpc)
              r (json/write-str r)
              r (format "Content-Length: %s\r\n\r\n%s" (alength (.getBytes r)) r)]

          (print r)

          (flush))

        :else
        (do
          (log "Append header")

          (swap! header-ref conj @line-ref))))))


(comment

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
