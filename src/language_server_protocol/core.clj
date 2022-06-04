(ns language-server-protocol.core
  (:require
   [clojure.string :as str]
   [clojure.data.json :as json]
   [clojure.java.io :as io])

  (:import
   (java.io
    File
    StringReader
    StringWriter)

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

(defn parse-content [header]
  (let [{:keys [Content-Length]} header

        writer (StringWriter.)]

    (io/copy *in* writer :buffer-size Content-Length)

    (.toString writer)))

(defn -main [& _]
  (let [char-ref (atom nil)

        parser-state-ref (atom {:chars []
                                :newline# 0})]

    (while (not= (reset! char-ref (.read *in*)) -1)
      (let [{:keys [chars newline#]} @parser-state-ref

            char (char @char-ref)

            newline? (= (char-name-string char) "newline")

            chars (conj chars char)]

        (cond
          ;; Two consecutive newline characters - parse header and content.
          (and newline? (= newline# 1))
          (let [header (parse-header chars)

                _ (log header)

                jsonrpc (parse-content header)

                _ (log jsonrpc)

                r {:id 0
                   :jsonrpc "2.0"
                   :result
                   {:capabilities
                    {:hoverProvider true}

                    :serverInfo
                    {:name "Nightincode"}}}

                r (json/write-str r)

                r (format "Content-Length: %s\r\n\r\n%s" (alength (.getBytes r)) r)]

            (print r)

            (flush)

            (reset! parser-state-ref {:chars []
                                      :newline# 0}))

          :else
          (reset! parser-state-ref {:chars chars
                                    ;; Reset newline counter when next character is part of the header.
                                    :newline# (if newline?
                                                (inc newline#)
                                                0)}))))))


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
