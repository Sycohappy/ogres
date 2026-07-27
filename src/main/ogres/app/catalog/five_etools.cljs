(ns ogres.app.catalog.five-etools
  "Maps private 5etools JSON records into Ogres v2 sheet fragments.
  Does not ship WotC text — callers load JSON from a private path or file import."
  (:require [clojure.string :as str]
            [ogres.app.character-sheet :as sheet]))

(defn source-id
  "Stable catalog source reference, e.g. 5etools:classFeature:paladin|Lay on Hands|PHB"
  [kind name source]
  (str "5etools:" kind ":"
       (str/join "|" (remove str/blank? [(str name) (str source)]))))

(defn ^:private entries-text [entries]
  (cond
    (string? entries) entries
    (sequential? entries)
    (->> entries
         (map (fn [e]
                (cond
                  (string? e) e
                  (map? e) (or (:entries e) (:entry e) (pr-str e))
                  :else (str e))))
         (str/join " "))
    :else ""))

(defn class-feature->v2
  "Convert a 5etools classFeature object into a v2 feature (+ optional resource)."
  [feature]
  (let [name (or (:name feature) "Feature")
        source (or (:source feature) "Unknown")
        entries (entries-text (or (:entries feature) []))
        uses (or (:uses feature) (:resource feature))
        economy (or (some-> feature :actionType keyword)
                    (some-> feature :economy keyword)
                    :trait)
        id (str/lower-case (str/replace name #"\s+" "-"))
        resource (when (or (number? uses) (map? uses))
                   (let [max-n (cond
                                 (number? uses) uses
                                 (number? (:value uses)) (:value uses)
                                 :else 1)]
                     {:id id
                      :name name
                      :kind (if (> max-n 5) "pool" "uses")
                      :max max-n
                      :recharge (or (:recharge feature) "long-rest")}))]
    {:feature (cond-> {:id id
                       :name name
                       :economy economy
                       :entries [entries]
                       :source (source-id "classFeature" name source)}
                resource (assoc :resourceId id))
     :resource resource}))

(defn item->v2
  "Convert a 5etools item into inventory entry + optional attack stub."
  [item]
  (let [name (or (:name item) "Item")
        source (or (:source item) "Unknown")
        id (str/lower-case (str/replace name #"\s+" "-"))
        dmg1 (:dmg1 item)
        dmg2 (:dmg2 item)
        bonus (or (:bonusWeapon item) (:bonusWeaponAttack item))
        damage (cond-> []
                 dmg1 (conj (or (when-let [[_ c s]
                                           (re-find #"(\d+)d(\d+)" (str dmg1))]
                                  {:id "1h" :count (js/parseInt c 10) :sides (js/parseInt s 10)
                                   :modifier 0 :type (or (:dmgType item) "bludgeoning")})
                                {:id "1h" :label (str dmg1) :count 1 :sides 6 :modifier 0}))
                 dmg2 (conj (or (when-let [[_ c s]
                                           (re-find #"(\d+)d(\d+)" (str dmg2))]
                                  {:id "2h" :count (js/parseInt c 10) :sides (js/parseInt s 10)
                                   :modifier 0 :type (or (:dmgType item) "bludgeoning")})
                                {:id "2h" :label (str dmg2) :count 1 :sides 8 :modifier 0})))
        attack (when (or (seq damage) bonus)
                 (cond-> {:id id
                          :name name
                          :kind "attack"
                          :description (entries-text (or (:entries item) []))
                          :source (source-id "item" name source)}
                   bonus (assoc :bonus (js/parseInt (str bonus) 10))
                   (seq damage) (assoc :damage damage)))]
    {:item {:id id
            :name name
            :attuned (boolean (:reqAttune item))
            :source (source-id "item" name source)
            :entries [(entries-text (or (:entries item) []))]}
     :attack attack}))

(defn spell->v2
  "Convert a 5etools spell into a prepared-spell reference fragment."
  [spell]
  (let [name (or (:name spell) "Spell")
        source (or (:source spell) "Unknown")
        level (or (:level spell) 0)]
    {:id (str/lower-case (str/replace name #"\s+" "-"))
     :name name
     :level level
     :school (:school spell)
     :entries [(entries-text (or (:entries spell) []))]
     :source (source-id "spell" name source)}))

(defn race-trait->v2
  [trait]
  (let [name (or (:name trait) "Trait")
        source (or (:source trait) "Unknown")]
    {:id (str/lower-case (str/replace name #"\s+" "-"))
     :name name
     :economy :trait
     :entries [(entries-text (or (:entries trait) []))]
     :source (source-id "raceTrait" name source)}))

(defn index-records
  "Build a flat searchable catalog from loaded 5etools JSON blobs.
  Accepts maps with keys like :class :classFeature :item :spell :race."
  [data]
  (vec
   (concat
    (for [f (or (:classFeature data) (:classFeatures data) [])]
      (let [{:keys [feature]} (class-feature->v2 f)]
        {:kind :classFeature :name (:name feature) :record f :v2 feature}))
    (for [i (or (:item data) (:items data) [])]
      (let [{:keys [item]} (item->v2 i)]
        {:kind :item :name (:name item) :record i :v2 item}))
    (for [s (or (:spell data) (:spells data) [])]
      (let [v2 (spell->v2 s)]
        {:kind :spell :name (:name v2) :record s :v2 v2}))
    (for [r (or (:race data) (:races data) [])
          t (or (:entries r) (:trait r) [])
          :when (map? t)]
      (let [v2 (race-trait->v2 (assoc t :source (:source r)))]
        {:kind :raceTrait :name (:name v2) :record t :v2 v2})))))

(defn search
  [catalog query]
  (let [q (str/lower-case (or query ""))]
    (if (str/blank? q)
      (vec (take 50 catalog))
      (filterv #(str/includes? (str/lower-case (str (:name %))) q) catalog))))

(defn apply-to-sheet
  "Applies a catalog entry onto a v2 sheet, returning the updated sheet."
  [sheet catalog-entry]
  (let [sheet (sheet/ensure-runtime (if (sheet/v2? sheet) sheet (sheet/convert-v1->v2 sheet)))
        kind (:kind catalog-entry)
        record (:record catalog-entry)]
    (case kind
      :classFeature
      (let [{:keys [feature resource]} (class-feature->v2 record)]
        (cond-> (update sheet :features (fnil conj []) feature)
          resource (update :resources (fnil conj []) resource)))

      :item
      (let [{:keys [item attack]} (item->v2 record)]
        (cond-> (update-in sheet [:inventory :items] (fnil conj []) item)
          attack (update :attacks (fnil conj []) attack)))

      :spell
      (let [spell (spell->v2 record)
            level (str (:level spell))]
        (update sheet :spellcasting
                (fn [blocks]
                  (let [blocks (vec (or blocks [{:name "Spells" :prepared {} :slots {}}]))
                        block (update (first blocks) :prepared
                                      (fn [prep]
                                        (update (or prep {}) level (fnil conj []) (:name spell))))]
                    (assoc blocks 0 block)))))

      :raceTrait
      (update sheet :features (fnil conj []) (race-trait->v2 record))

      sheet)))
