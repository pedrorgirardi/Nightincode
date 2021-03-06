(ns dev
  (:require
   [nightincode.server :as server]
   [datascript.core :as d]))

(comment


  (keys (:LSP/document @server/state-ref))


  (def conn (server/_analyzer-conn @server/state-ref))

  ;; Every Namespace:
  (d/q '[:find  [(pull ?v [:semthetic/identifier]) ...]
         :where
         [?v :semthetic/semantic :namespace]]
    (d/db conn))

  ;; Every Namespace usage:
  (d/q '[:find  [(pull ?v [:semthetic/identifier]) ...]
         :where
         [?v :semthetic/semantic :namespace]
         [?v :semthetic/modifier :usage]]
    (d/db conn))

  ;; A particular namespace usage:
  (d/q '[:find  [(pull ?v [*]) ...]
         :where
         [?v :semthetic/semantic :namespace]
         [?v :semthetic/modifier :usage]
         [?v :semthetic/identifier nightincode.analyzer]]
    (d/db conn))

  ;; Every Var:
  (d/q '[:find  [(pull ?v [:semthetic/identifier]) ...]
         :where
         [?v :semthetic/semantic :var]]
    (d/db conn))

  ;; Every Var usage:
  (d/q '[:find  ?identifier
         :where
         [?v :semthetic/semantic :var]
         [?v :semthetic/modifier :usage]
         [?v :semthetic/identifier ?identifier]]
    (d/db conn))

  ;; Var usage missing name row & rol:
  (d/q '[:find  [(pull ?v [*]) ...]
         :where
         [?v :semthetic/semantic :var]
         [(missing? $ ?v :var-usage/name-row)]]
    (d/db conn))

  ;; Every local:
  (d/q '[:find  [(pull ?v [*]) ...]
         :where
         [?v :semthetic/semantic :local]]
    (d/db conn))

  ;; Every local usage:
  (d/q '[:find  [(pull ?v [*]) ...]
         :where
         [?v :semthetic/semantic :local]
         [?v :semthetic/modifier :usage]]
    (d/db conn))

  ;; Local usage missing name row & rol:
  (d/q '[:find  [(pull ?v [*]) ...]
         :where
         [?v :semthetic/semantic :local]
         [?v :semthetic/modifier :usage]
         [(missing? $ ?v :local-usage/name-row)]]
    (d/db conn))


  )
