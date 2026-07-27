(ns ogres.app.catalog.core
  "In-memory catalog store for privately imported 5etools JSON."
  (:require [ogres.app.catalog.five-etools :as five]))

(defonce ^:private state (atom {:catalog [] :source nil}))

(defn catalog []
  (:catalog @state))

(defn source []
  (:source @state))

(defn load-data!
  "Replace catalog from a parsed 5etools-shaped JSON map. source is a label only."
  [data source-label]
  (let [indexed (five/index-records data)]
    (reset! state {:catalog indexed :source source-label})
    {:count (count indexed) :source source-label}))

(defn clear!
  []
  (reset! state {:catalog [] :source nil}))

(defn search [query]
  (five/search (catalog) query))

(defn apply-entry [sheet entry]
  (five/apply-to-sheet sheet entry))
