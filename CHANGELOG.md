# CHANGELOG

All notable changes to this project will be documented in this file.

## 0.15.0 - in development
- Updated all dependencies

---

## 0.14.0 - 2024-02-13
- Updated clj-kondo/clj-kondo
- Updated org.clojure/tools.logging

---

## 0.13.1 - 2024-01-10
- Fix file URI in diagnostics

---

## 0.13.0 - 2024-01-08
- New Command `nightincode.dump` & LSP method `nightincode/dump`

---

## 0.12.0 - 2024-01-06
- Diagnostics improvements and bug fixes
- Analyze classpath/dependencies - it's persisted on a different database

---

## 0.11.0 - 2024-01-03
- Create `.clj-kondo` directory if it doesn't exist:
	Nightincode should 'just work' if there's a `deps.edn` file.

---

## 0.10.0 - 2024-01-02
- Analyze `paths` and `extra-paths` from `deps.edn`
