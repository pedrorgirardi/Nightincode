(ns nightincode.extension
  (:require
   ["vscode" :as vscode]))

(def word-pattern
  "Clojure symbol regex."
  #"(?:/|[^\s,;\(\)\[\]{}\"`~@\^\\][^\s,;\(\)\[\]{}\"`~@\^\\]*)")

(defn activate [^js context]
  (let [^js languages (.-languages vscode)]
    (.setLanguageConfiguration languages "clojure" #js {:wordPattern word-pattern})))

(defn deactivate []
  nil)
