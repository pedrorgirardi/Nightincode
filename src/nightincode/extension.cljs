(ns nightincode.extension
  (:require
   ["vscode" :as vscode]
   ["path" :as path]
   ["vscode-languageclient/node" :as client]))

(def state-ref (atom nil))

(def word-pattern
  "Clojure symbol regex."
  #"(?:/|[^\s,;\(\)\[\]{}\"`~@\^\\][^\s,;\(\)\[\]{}\"`~@\^\\]*)")

(defn activate [^js context]
  (let [^js output (vscode/window.createOutputChannel "Nightincode")

        server-JAR-path (path/join (.-extensionPath context) "nightincode.jar")

        server-options #js{:run #js{:command "java" :args #js["-jar", server-JAR-path]}
                           :debug #js{:command "java" :args #js["-jar", server-JAR-path]}}

        client-options #js{:documentSelector #js[#js{:language "clojure"}]
                           :outputChannel output}

        client (client/LanguageClient. "nightincode" "Nightincode" server-options client-options)

        ^js subscriptions (.-subscriptions context)]

    (reset! state-ref {:client client})

    (vscode/languages.setLanguageConfiguration "clojure" #js {:wordPattern word-pattern})

    (.appendLine output (str "Extension Path: " (.-extensionPath context) "\n"))

    (.push subscriptions (.start client))

    (js/console.log "Activated Nightincode")))

(defn deactivate []
  (when-let [^js client (:client @state-ref)]
    (.stop client))

  (js/console.log "Deactivated Nightincode"))
