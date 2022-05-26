(ns language-server-protocol.core
  (:require
   [clojure.string :as str]
   [clojure.data.json :as json])

  (:import
   (java.io
    PipedInputStream
    PipedOutputStream)

   (java.util.concurrent
    CompletableFuture)

   (org.eclipse.lsp4j.jsonrpc
    Endpoint))

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
