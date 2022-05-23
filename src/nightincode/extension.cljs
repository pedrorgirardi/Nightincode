(ns nightincode.extension
  (:require
   ["vscode" :as vscode]
   ["path" :as path]
   ["vscode-languageclient/node" :as client]))

(def word-pattern
  "Clojure symbol regex."
  #"(?:/|[^\s,;\(\)\[\]{}\"`~@\^\\][^\s,;\(\)\[\]{}\"`~@\^\\]*)")

#_(defn activate [^js context]
    (let [^js output (vscode/window.createOutputChannel "Nightincode")

          server-JAR-path (path/join (.-extensionPath context) "server.jar")

          server-options #js{:run #js{:command "java" :args #js["-jar", server-JAR-path]}
                             :debug #js{:command "java" :args #js["-jar", server-JAR-path]}}

          client-options #js{:documentSelector #js[#js{:language "clojure"}]
                             :outputChannel output}

          client (client/LanguageClient. "Nightincode" "Nightincode" server-options client-options)

          ^js subscriptions (.-subscriptions context)]

      (vscode/languages.setLanguageConfiguration "clojure" #js {:wordPattern word-pattern})

      (.appendLine output (str "Extension Path: " (.-extensionPath context) "\n"))

      (.push subscriptions (.start client))))

(defn activate [^js context]
  (let [^js output (vscode/window.createOutputChannel "Nightincode")]

    (vscode/languages.setLanguageConfiguration "clojure" #js {:wordPattern word-pattern})

    (.appendLine output (str "Extension Path: " (.-extensionPath context) "\n"))))

(defn deactivate []
  nil)
