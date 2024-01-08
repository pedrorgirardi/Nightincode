(ns nightincode.extension
  (:require
   [clojure.pprint :as pprint]

   ["vscode" :as vscode]
   ["path" :as path]
   ["vscode-languageclient/node" :as client]))

(def state-ref (atom nil))

(defn _client
  "Returns the language client instance."
  [state]
  (:client state))

(defn _output-channel [state]
  (:output-channel state))

(def word-pattern
  "Clojure symbol regex."
  #"(?:/|[^\s,;\(\)\[\]{}\"`~@\^\\][^\s,;\(\)\[\]{}\"`~@\^\\]*)")

(defn- register-command [name cmd]
  (-> (.-commands ^js vscode)
    (.registerCommand name
      (fn []
        (js/console.log (str "[Nightincode] RUN COMMAND '" name "'"))
        (try
          (cmd)
          (catch js/Error e
            (js/console.error (str "[Nightincode] FAILED TO RUN COMMAND '" name "'") e)))))))

(defn- register-disposable [^js context ^js disposable]
  (-> (.-subscriptions context)
      (.push disposable)))


(defn cmd-debug-state []
  (when-let [r (.sendRequest (_client @state-ref) "nightincode/debugState" (clj->js {:foo :bar}))]
    (.then r
      (fn [result]
        (let [openTextDocument (vscode/workspace.openTextDocument
                                 #js {:content (js/JSON.stringify result nil 2)
                                      :language "json"})]

          (.then openTextDocument
            (fn [document]
              (vscode/window.showTextDocument document))))))))


(defn activate [^js context]
  (let [^js output (vscode/window.createOutputChannel "Nightincode")

        server-JAR-path (path/join (.-extensionPath context) "nightincode.jar")

        server-options #js{:run #js{:command "java" :args #js["-jar", server-JAR-path]}
                           :debug #js{:command "java" :args #js["-jar", server-JAR-path]}}

        client-options #js{:documentSelector #js[#js{:language "clojure"}]
                           :outputChannel output}

        client (client/LanguageClient. "nightincode" "Nightincode" server-options client-options)

        ^js subscriptions (.-subscriptions context)]

    (reset! state-ref
      {:client client
       :output-channel output})

    (vscode/languages.setLanguageConfiguration "clojure" #js {:wordPattern word-pattern})

    (.push subscriptions (.start client))

    (->>
      (register-command "nightincode.debugState" cmd-debug-state)
      (register-disposable context))

    (js/console.log "Activated Nightincode")))

(defn deactivate []
  (when-let [^js client (_client @state-ref)]
    (.stop client))

  (js/console.log "Deactivated Nightincode"))
