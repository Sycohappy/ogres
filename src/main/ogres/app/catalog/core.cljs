(ns ogres.app.catalog.core
  "In-memory catalog store for privately imported 5etools JSON."
  (:require [clojure.string :as str]
            [ogres.app.catalog.five-etools :as five]))

(defonce ^:private state (atom {:catalog [] :sources []}))

(defn catalog []
  (:catalog @state))

(defn source []
  (let [sources (:sources @state)]
    (when (seq sources)
      (str/join ", " sources))))

(defn sources []
  (:sources @state))

(defn ^:private entry-key [entry]
  (or (get-in entry [:v2 :source])
      (str (name (:kind entry)) ":" (:name entry))))

(defn ^:private merge-catalog [existing incoming]
  (vec (vals (merge (into {} (map (juxt entry-key identity) existing))
                    (into {} (map (juxt entry-key identity) incoming))))))

(defn load-data!
  "Load catalog from a parsed 5etools-shaped JSON map.
   When merge? is true, append/replace by source id instead of clearing."
  ([data source-label]
   (load-data! data source-label false))
  ([data source-label merge?]
   (let [indexed (five/index-records data)
         label (or source-label "catalog.json")]
     (if merge?
       (let [next (merge-catalog (:catalog @state) indexed)
             sources (-> (:sources @state)
                         (conj label)
                         distinct
                         vec)]
         (reset! state {:catalog next :sources sources})
         {:count (count next) :added (count indexed) :source label :merged true})
       (do
         (reset! state {:catalog indexed :sources [label]})
         {:count (count indexed) :added (count indexed) :source label :merged false})))))

(defn clear!
  []
  (reset! state {:catalog [] :sources []}))

(defn search [query]
  (five/search (catalog) query))

(defn apply-entry [sheet entry]
  (five/apply-to-sheet sheet entry))
