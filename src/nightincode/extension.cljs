(ns nightincode.extension
  (:require
   ["vscode" :as vscode]
   ["path" :as path]
   ["vscode-languageclient/node" :as client]))

(def word-pattern
  "Clojure symbol regex."
  #"(?:/|[^\s,;\(\)\[\]{}\"`~@\^\\][^\s,;\(\)\[\]{}\"`~@\^\\]*)")

(defn activate [^js context]
  (let [^js output (vscode/window.createOutputChannel "Nightincode")

        jar-path (path/join (.-extensionPath context) "server.jar")

        server-options #js{:run #js{:command "java" :args #js["-jar", jar-path]}
                           :debug #js{:command "java" :args #js["-jar", jar-path]}}

        client-options #js{:documentSelector #js[#js{:language "clojure"}]
                           :outputChannel output}]

    (vscode/languages.setLanguageConfiguration "clojure" #js {:wordPattern word-pattern})

    (.appendLine output (str "Extension Path: " (.-extensionPath context) "\n"))

    (.appendLine output "Happy coding!")))

(defn deactivate []
  nil)
