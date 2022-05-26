(ns language-server-protocol.core
  (:require
   [clojure.string :as str]
   [clojure.data.json :as json])
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

