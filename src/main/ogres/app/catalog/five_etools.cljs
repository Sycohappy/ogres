(ns ogres.app.catalog.five-etools
  "Maps private 5etools JSON records into Ogres v2 sheet fragments.
  Does not ship WotC text — callers load JSON from a private path or file import."
  (:require [clojure.string :as str]
            [ogres.app.character-sheet :as sheet]))

(def ^:private school-names
  {"A" "Abjuration"
   "C" "Conjuration"
   "D" "Divination"
   "E" "Enchantment"
   "V" "Evocation"
   "I" "Illusion"
   "N" "Necromancy"
   "T" "Transmutation"})

(defn source-id
  "Stable catalog source reference, e.g. 5etools:classFeature:paladin|Lay on Hands|PHB"
  [kind name source]
  (str "5etools:" kind ":"
       (str/join "|" (remove str/blank? [(str name) (str source)]))))

(defn strip-tags
  "Remove 5etools {@tag …} markup, keeping readable display text."
  [text]
  (when (string? text)
    (-> text
        (str/replace #"\{@damage ([^}]+)\}" "$1")
        (str/replace #"\{@dice ([^}]+)\}" "$1")
        (str/replace #"\{@scaledamage ([^|}]+)[^}]*\}" "$1")
        (str/replace #"\{@h\}" "")
        (str/replace #"\{@[a-zA-Z0-9]+ ([^|}]+)(?:\|[^}]*)?\}" "$1")
        (str/replace #"\{@[^}]+\}" "")
        str/trim)))

(defn ^:private entries-text [entries]
  (cond
    (string? entries) (or (strip-tags entries) "")
    (sequential? entries)
    (->> entries
         (map (fn [e]
                (cond
                  (string? e) (or (strip-tags e) "")
                  (map? e) (or (when (string? (:entries e)) (strip-tags (:entries e)))
                               (when (sequential? (:entries e))
                                 (entries-text (:entries e)))
                               (:entry e)
                               (:name e)
                               "")
                  :else (str e))))
         (remove str/blank?)
         (str/join " "))
    :else ""))

(defn ^:private raw-entries-text [entries]
  (cond
    (string? entries) entries
    (sequential? entries)
    (->> entries
         (map (fn [e]
                (cond
                  (string? e) e
                  (map? e) (raw-entries-text (or (:entries e) []))
                  :else "")))
         (remove str/blank?)
         (str/join " "))
    :else ""))

(defn ^:private parse-damage-tags
  "Pull {@damage NdM[+K]} tags from raw entry strings into structured dice."
  [entries damage-types]
  (let [raw (raw-entries-text entries)
        dtype (some-> damage-types first str str/lower-case)
        matches (re-seq #"\{@damage\s+(\d+)d(\d+)(?:\s*\+\s*(\d+))?\}" (or raw ""))]
    (vec
     (map-indexed
      (fn [idx [_ c s m]]
        (cond-> {:id (str "dmg-" idx)
                 :count (js/parseInt c 10)
                 :sides (js/parseInt s 10)
                 :modifier (js/parseInt (or m "0") 10)}
          dtype (assoc :type dtype)))
      matches))))

(defn ^:private format-time [time]
  (cond
    (string? time) time
    (sequential? time)
    (->> time
         (map (fn [t]
                (cond
                  (string? t) t
                  (map? t)
                  (str (or (:number t) 1) " "
                       (or (:unit t) "action")
                       (when (:condition t) (str " (" (:condition t) ")")))
                  :else (str t))))
         (str/join "; "))
    :else nil))

(defn ^:private format-range [range]
  (cond
    (string? range) range
    (map? range)
    (let [dist (:distance range)
          amount (or (:amount dist) (:distance dist))
          dtype (or (:type dist) (:type range))]
      (cond
        (and amount dtype) (str amount " " dtype)
        (= (:type range) "special") "Special"
        (= (:type range) "self") "Self"
        (= (:type range) "touch") "Touch"
        :else (or (entries-text [range]) nil)))
    :else nil))

(defn ^:private format-components [components]
  (cond
    (string? components) components
    (map? components)
    (->> [(when (:v components) "V")
          (when (:s components) "S")
          (when-let [m (:m components)]
            (str "M (" (cond (string? m) m
                             (map? m) (or (:text m) (pr-str m))
                             :else (str m)) ")"))]
         (remove nil?)
         (str/join ", "))
    :else nil))

(defn ^:private format-duration [duration]
  (cond
    (string? duration) duration
    (sequential? duration)
    (->> duration
         (map (fn [d]
                (cond
                  (string? d) d
                  (map? d)
                  (let [inner (:duration d)
                        amount (or (:amount inner) (:number inner))
                        unit (or (:type inner) (:unit inner))]
                    (str (when (:concentration d) "Concentration, up to ")
                         (or (when (and amount unit) (str amount " " unit))
                             (:type d)
                             "instantaneous")))
                  :else (str d))))
         (str/join "; "))
    :else nil))

(defn ^:private concentration? [spell]
  (boolean
   (or (:concentration spell)
       (some #(and (map? %) (:concentration %)) (or (:duration spell) [])))))

(defn ^:private ritual? [spell]
  (boolean
   (or (:ritual spell)
       (some #(or (and (map? %) (:ritual %))
                  (= % "ritual")
                  (= (:ritual %) true))
             (or (:meta spell) [])))))

(defn ^:private school-label [school]
  (or (get school-names (str school))
      (when (string? school) school)
      nil))

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
  "Convert a 5etools spell into a structured spell fragment for the sheet index."
  [spell]
  (let [name (or (:name spell) "Spell")
        source (or (:source spell) "Unknown")
        level (or (:level spell) 0)
        body (entries-text (or (:entries spell) []))
        higher (entries-text (or (:entriesHigherLevel spell) (:higherLevel spell) []))
        entries (cond-> []
                  (not (str/blank? body)) (conj body)
                  (not (str/blank? higher)) (conj (str "At Higher Levels. " higher)))
        saves (mapv str (or (:savingThrow spell) []))
        spell-atk (boolean (seq (:spellAttack spell)))
        damage (parse-damage-tags (or (:entries spell) [])
                                  (or (:damageInflict spell) []))]
    (cond-> {:id (str/lower-case (str/replace name #"\s+" "-"))
             :name name
             :level level
             :school (school-label (:school spell))
             :entries entries
             :source (source-id "spell" name source)
             :concentration (concentration? spell)
             :ritual (ritual? spell)
             :spellAttack spell-atk}
      (seq saves) (assoc :savingThrow saves)
      (seq damage) (assoc :damage damage)
      (format-time (:time spell)) (assoc :time (format-time (:time spell)))
      (format-range (:range spell)) (assoc :range (format-range (:range spell)))
      (format-components (:components spell)) (assoc :components (format-components (:components spell)))
      (format-duration (:duration spell)) (assoc :duration (format-duration (:duration spell))))))

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
        {:kind :spell :name (:name v2) :level (:level v2) :record s :v2 v2}))
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
      (filterv (fn [entry]
                 (or (str/includes? (str/lower-case (str (:name entry))) q)
                     (str/includes? (str/lower-case (name (:kind entry))) q)))
               catalog))))

(defn ^:private prepared-has-name? [names spell-name]
  (let [want (str/lower-case (str spell-name))]
    (some (fn [n]
            (= want (str/lower-case (str (if (map? n) (:name n) n)))))
          names)))

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
            level (str (:level spell))
            spell-name (:name spell)]
        (-> sheet
            (update :spellIndex
                    (fn [idx]
                      (assoc (or idx {}) (:id spell) spell)))
            (update :spellcasting
                    (fn [blocks]
                      (let [blocks (vec (or blocks [{:name "Spells" :prepared {} :slots {}}]))
                            block0 (or (first blocks) {:name "Spells" :prepared {} :slots {}})
                            prep (or (:prepared block0) {})
                            names (or (get prep level)
                                      (get prep (keyword level))
                                      [])
                            names' (if (prepared-has-name? names spell-name)
                                     (vec names)
                                     (conj (vec names) spell-name))
                            block' (assoc block0 :prepared (assoc prep level names'))]
                        (assoc blocks 0 block'))))))

      :raceTrait
      (update sheet :features (fnil conj []) (race-trait->v2 record))

      sheet)))
