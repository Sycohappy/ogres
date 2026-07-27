(ns ogres.app.character-sheet
  "V2 character sheet adapter: dual-read structured fields with v1 prose fallback."
  (:require [clojure.string :as str]
            [ogres.app.initiative :as initiative]))

(defn v2?
  "True when the sheet uses the versioned PC schema."
  [sheet]
  (or (= 2 (:version sheet))
      (= "2" (str (:version sheet)))))

(defn sheet-name
  [sheet]
  (or (get-in sheet [:identity :name])
      (:name sheet)
      "Unnamed"))

(defn ^:private ability-mod [score]
  (when (number? score)
    (js/Math.floor (/ (- score 10) 2))))

(defn ability-score
  "Effective ability score for kw (:str … :cha)."
  [sheet kw]
  (if (v2? sheet)
    (or (get-in sheet [:abilities kw :score])
        (get-in sheet [:abilities (name kw) :score]))
    (get sheet kw)))

(defn ability-modifier
  [sheet kw]
  (or (when (v2? sheet)
        (get-in sheet [:abilities kw :modifier]))
      (ability-mod (ability-score sheet kw))
      0))

(defn ability-save
  [sheet kw]
  (if (v2? sheet)
    (or (get-in sheet [:abilities kw :save])
        (+ (ability-modifier sheet kw)
           (if (get-in sheet [:abilities kw :proficient])
             (or (get-in sheet [:vitals :proficiencyBonus]) 0)
             0)))
    (ability-modifier sheet kw)))

(defn abilities-map
  "Map of :str…:cha → {:score :modifier :save :proficient}."
  [sheet]
  (into {}
        (for [kw [:str :dex :con :int :wis :cha]
              :let [score (ability-score sheet kw)]
              :when (number? score)]
          [kw {:score score
               :modifier (ability-modifier sheet kw)
               :save (ability-save sheet kw)
               :proficient (boolean (get-in sheet [:abilities kw :proficient]))}])))

(defn proficiency-bonus
  [sheet]
  (or (get-in sheet [:vitals :proficiencyBonus])
      (when-let [pb (:proficiency-bonus sheet)]
        (initiative/parse-modifier pb))
      0))

(defn skills-map
  "Normalized skills → {:modifier :proficient :ability}."
  [sheet]
  (cond
    (v2? sheet)
    (into {}
          (for [[k v] (or (:skills sheet) {})]
            [(keyword k)
             (if (map? v)
               {:modifier (or (:modifier v) (initiative/parse-modifier (:modifier v)) 0)
                :proficient (boolean (:proficient v))
                :ability (some-> (:ability v) keyword)}
               {:modifier (or (initiative/parse-modifier v) 0)
                :proficient false
                :ability nil})]))

    (map? (:skill sheet))
    (into {}
          (for [[k v] (:skill sheet)]
            [(keyword k)
             {:modifier (or (initiative/parse-modifier v) 0)
              :proficient true
              :ability nil}]))

    :else {}))

(defn ac-value
  [sheet]
  (cond
    (v2? sheet) (or (get-in sheet [:vitals :ac :value])
                    (get-in sheet [:vitals :ac]))
    (number? (:ac sheet)) (:ac sheet)
    (sequential? (:ac sheet))
    (let [first-ac (first (:ac sheet))]
      (cond
        (number? first-ac) first-ac
        (map? first-ac) (or (:ac first-ac) (:value first-ac))
        :else nil))
    (map? (:ac sheet)) (or (get-in sheet [:ac :ac]) (get-in sheet [:ac :value]))
    :else nil))

(defn ac-from
  [sheet]
  (or (get-in sheet [:vitals :ac :from])
      (when (sequential? (:ac sheet))
        (some :from (:ac sheet)))
      []))

(defn hp-max
  [sheet]
  (or (get-in sheet [:vitals :hp :max])
      (get-in sheet [:hp :average])
      (get-in sheet [:hp :max])))

(defn hp-formula
  [sheet]
  (or (get-in sheet [:vitals :hp :formula])
      (get-in sheet [:hp :formula])))

(defn ensure-runtime
  "Ensures runtime HP/resources exist; returns updated sheet."
  [sheet]
  (let [max-hp (or (hp-max sheet) 0)
        runtime (or (:runtime sheet) {})
        hp (or (:hp runtime) {})]
    (assoc sheet :runtime
           (-> runtime
               (assoc :hp {:current (or (:current hp) max-hp)
                           :temp (or (:temp hp) 0)})
               (update :slotsExpended #(or % {}))
               (update :resourceSpent #(or % {}))
               (update :effects #(or % []))
               (update :hitDiceSpent #(or % 0))))))

(defn runtime-hp
  [sheet]
  (let [sheet (ensure-runtime sheet)]
    (get-in sheet [:runtime :hp])))

(defn speed-map
  [sheet]
  (or (get-in sheet [:vitals :speed])
      (:speed sheet)
      {}))

(defn initiative-bonus
  [sheet]
  (cond
    (v2? sheet)
    (or (get-in sheet [:vitals :initiative :bonus])
        (ability-modifier sheet :dex)
        0)

    (get-in sheet [:initiative :bonus])
    (or (initiative/parse-modifier (get-in sheet [:initiative :bonus])) 0)

    :else
    (or (ability-modifier sheet :dex) 0)))

(defn senses
  [sheet]
  (or (get-in sheet [:vitals :senses])
      (:senses sheet)
      []))

(defn languages
  [sheet]
  (or (get-in sheet [:identity :languages])
      (:languages sheet)
      []))

(defn resistances
  [sheet]
  (or (get-in sheet [:vitals :resistances])
      (:resist sheet)
      []))

(defn ^:private entry->feature
  [economy {:keys [name entries id resourceId source]}]
  (cond-> {:id (or id (str (str/lower-case (or name "feature")) "-" (name economy)))
           :name (or name "Feature")
           :economy (keyword economy)
           :entries (or entries [])}
    resourceId (assoc :resourceId resourceId)
    source (assoc :source source)))

(defn features
  "All features with :economy. Dual-read from v2 or v1 trait/action/bonus/reaction."
  [sheet]
  (if (v2? sheet)
    (vec (or (:features sheet) []))
    (vec
     (concat
      (map #(entry->feature :trait %) (or (:trait sheet) []))
      (map #(entry->feature :action %) (or (:action sheet) []))
      (map #(entry->feature :bonus %) (or (:bonus sheet) []))
      (map #(entry->feature :reaction %) (or (:reaction sheet) []))
      (map #(entry->feature :legendary %) (or (:legendary sheet) []))))))

(defn features-by-economy
  [sheet economy]
  (filterv #(= (keyword economy) (keyword (:economy %))) (features sheet)))

(defn resources
  [sheet]
  (vec (or (:resources sheet) [])))

(defn resource-by-id
  [sheet id]
  (some #(when (= (str (:id %)) (str id)) %) (resources sheet)))

(defn resource-spent
  [sheet id]
  (or (get-in (ensure-runtime sheet) [:runtime :resourceSpent (keyword id)])
      (get-in (ensure-runtime sheet) [:runtime :resourceSpent (str id)])
      0))

(defn resource-remaining
  [sheet id]
  (let [res (resource-by-id sheet id)
        max-n (or (:max res) 0)
        spent (resource-spent sheet id)]
    (max 0 (- max-n spent))))

(defn ^:private parse-damage-token
  "Parses '1d8+4 bludgeoning' or '2d10 fire' into a damage option map."
  [text]
  (when-let [[_ count sides mod type]
             (re-find #"(?i)(\d+)d(\d+)(?:\s*\+\s*(\d+))?(?:\s+([A-Za-z]+))?" (str text))]
    (cond-> {:id (str count "d" sides (when mod (str "+" mod)))
             :count (js/parseInt count 10)
             :sides (js/parseInt sides 10)
             :modifier (or (initiative/parse-modifier mod) 0)}
      type (assoc :type (str/lower-case type)))))

(defn ^:private prose-attack
  [{:keys [name entries] :as entry}]
  (let [description (str/join " " (or entries []))
        bonus (initiative/attack-modifier description)
        ;; Build a synthetic attack body so damage parsers run without rolling.
        probe (str name ". " description
                   (when bonus (str " — 10 (d20 +" bonus ")")))
        exprs (or (initiative/parse-damage-expressions probe) [])]
    (cond-> {:id (str "v1-" (str/lower-case (or name "attack")))
             :name (or name "Attack")
             :kind (if (or bonus (seq exprs)) :attack :action)
             :description description
             :entries (or entries [])}
      bonus (assoc :bonus bonus)
      (seq exprs) (assoc :damage
                         (mapv (fn [expr i]
                                 (assoc expr
                                        :id (str "dmg-" i)
                                        :label (initiative/damage-button-label expr)))
                               exprs (range))))))

(defn attacks
  "Structured attacks; synthesizes from v1 action prose when needed."
  [sheet]
  (if (v2? sheet)
    (vec (or (:attacks sheet) []))
    (->> (or (:action sheet) [])
         (map prose-attack)
         (filterv #(or (= :attack (:kind %)) (seq (:damage %)))))))

(defn damage-options
  "Damage option maps for an attack (structured preferred)."
  [attack]
  (if (seq (:damage attack))
    (mapv (fn [d]
            (cond-> {:count (or (:count d) 1)
                     :sides (or (:sides d) 6)
                     :modifier (or (:modifier d) 0)
                     :id (:id d)
                     :label (:label d)}
              (:type d) (assoc :type (:type d))))
          (:damage attack))
    []))

(defn format-damage-option
  [{:keys [count sides modifier type] :as expr}]
  (initiative/damage-button-label
   (cond-> {:count (or count 1) :sides (or sides 6) :modifier (or modifier 0)}
     type (assoc :type type))))

(defn format-attack-description
  "Builds chat/prose description from a structured attack so damage parsers work."
  [attack]
  (let [bonus (:bonus attack)
        existing (or (:description attack)
                     (str/join " " (or (:entries attack) [])))
        dmgs (:damage attack)]
    (if (and (seq dmgs) (str/blank? existing))
      (let [hit (when bonus
                  (str "Melee Attack Roll: +" bonus ", one target. Hit: "))
            parts (map-indexed
                   (fn [i d]
                     (let [avg (+ (or (:modifier d) 0)
                                  (* (or (:count d) 1)
                                     (/ (inc (or (:sides d) 6)) 2)))
                           formula (str "(" (or (:count d) 1) "d" (or (:sides d) 6)
                                        (when (pos? (or (:modifier d) 0))
                                          (str " + " (:modifier d)))
                                        ")"
                                        (when (:type d) (str " " (:type d)))
                                        " damage")]
                       (str (js/Math.round avg) " " formula
                            (when (and (pos? i) (< i (count dmgs)))
                              (if (= (inc i) (count dmgs)) " if used with two hands" "")))))
                   dmgs)]
        (str hit (str/join ", or " parts)))
      (or existing ""))))

(defn attack-chat-body
  "Chat body for a structured or prose attack."
  [attack]
  (let [name (:name attack)
        description (format-attack-description attack)
        bonus (:bonus attack)]
    (if (number? bonus)
      (let [die (inc (rand-int 20))
            total (+ die bonus)
            base (if (str/blank? description)
                   name
                   (str name ". " description))]
        (str base " — " total " (d20 +" bonus ")"))
      (initiative/action-chat-body name description))))

(defn identity-block
  [sheet]
  (if (v2? sheet)
    (or (:identity sheet) {})
    {:name (:name sheet)
     :class (some-> (:type sheet) str/capitalize)
     :species (:subtype sheet)
     :alignment (:alignment sheet)
     :size (first (or (:size sheet) []))
     :level (initiative/parse-modifier (:cr sheet))}))

(defn spellcasting
  [sheet]
  (vec (or (:spellcasting sheet) [])))

(defn inventory
  [sheet]
  (or (:inventory sheet) {}))

(defn effects
  [sheet]
  (vec (get-in (ensure-runtime sheet) [:runtime :effects] [])))

(defn ^:private v1-entries->features [economy entries]
  (mapv #(entry->feature economy %) (or entries [])))

(defn convert-v1->v2
  "Converts a bestiary-style v1 sheet into a v2 PC sheet skeleton."
  [sheet]
  (if (v2? sheet)
    sheet
    (let [name (or (:name sheet) "Unnamed")
          max-hp (or (get-in sheet [:hp :average]) 0)
          pb (or (initiative/parse-modifier (:proficiency-bonus sheet)) 0)
          ac (ac-value sheet)
          attacks' (attacks sheet)
          traits (v1-entries->features :trait (:trait sheet))
          actions (->> (or (:action sheet) [])
                       (remove (fn [a]
                                 (some #(= (:name %) (:name a)) attacks')))
                       (v1-entries->features :action))
          bonuses (v1-entries->features :bonus (:bonus sheet))
          reactions (v1-entries->features :reaction (:reaction sheet))
          features' (vec (concat traits actions bonuses reactions))]
      (ensure-runtime
       {:version 2
        :identity {:name name
                   :class (some-> (:type sheet) str)
                   :species (:subtype sheet)
                   :alignment (:alignment sheet)
                   :size (first (or (:size sheet) []))
                   :level (or (initiative/parse-modifier (:cr sheet)) nil)
                   :languages (or (:languages sheet) [])}
        :vitals {:ac {:value ac :from (or (ac-from sheet) [])}
                 :hp {:max max-hp
                      :average max-hp
                      :formula (get-in sheet [:hp :formula])}
                 :speed (or (:speed sheet) {})
                 :initiative {:bonus (initiative-bonus sheet)}
                 :proficiencyBonus pb
                 :passivePerception (or (:passive sheet) 10)
                 :senses (or (:senses sheet) [])
                 :resistances (or (:resist sheet) [])}
        :abilities (into {}
                         (for [kw [:str :dex :con :int :wis :cha]
                               :let [score (get sheet kw)]
                               :when (number? score)]
                           [kw {:score score
                                :modifier (ability-mod score)
                                :save (ability-mod score)
                                :proficient false}]))
        :skills (skills-map sheet)
        :attacks attacks'
        :features features'
        :resources []
        :spellcasting (or (:spellcasting sheet) [])
        :inventory {}}))))

(defn display-name
  "Backward-compatible :name for events that assoc :name onto sheet maps."
  [sheet]
  (sheet-name sheet))

(defn with-display-name
  "Ensures top-level :name exists for library indexing / token link updates."
  [sheet]
  (assoc sheet :name (sheet-name sheet)))
