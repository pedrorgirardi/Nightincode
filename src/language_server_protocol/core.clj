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

(defn -main [& _]
  (while true
    (let [[_ jsonrpc] (str/split (slurp *in*) #"\r\n\r\n")

          {:strs [id]} (json/read-str jsonrpc)

          result (json/write-str {:id id
                                  :jsonrpc "2.0"
                                  :result
                                  {:capabilities
                                   {:hoverProvider true}

                                   :serverInfo
                                   {:name "Nightincode"}}})

          result (format "Content-Length: %s\r\n\r\n%s" (alength (.getBytes result)) result)]

      (pr result))))


(comment

  (def r
    (LineNumberingPushbackReader.
      (StringReader. "Foo1: 1\r\nFoo2: 2\r\n\r\n{}")))

  (.readLine r)

  (with-in-str "Foo1: 1\r\nFoo2: 1\r\n"
    (.readLine *in*))

  (with-in-str "\r\n\r\n"
    (.readLine *in*))


  (let [line-ref (atom nil)

        process-ref (atom {:header []
                           :blank? false})]

    (with-open [r (LineNumberingPushbackReader.
                    (StringReader. "Foo1: 1\r\nFoo2: 2\r\n\r\n{\"a\": 2}"))]

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
