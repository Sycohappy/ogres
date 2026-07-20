(ns ogres.app.import.parser
  (:require [clojure.string :as str]))

(def ^:private size-codes
  {"tiny" "T" "small" "S" "medium" "M" "large" "L" "huge" "H" "gargantuan" "G"})

(def ^:private alignment-codes
  {"lawful" "L" "neutral" "N" "chaotic" "C" "good" "G" "evil" "E" "unaligned" "U"})

(defn ^:private strip-blockquote [line]
  (str/trim (str/replace line #"^>\s?" "")))

(defn ^:private extract-stat-blocks [text]
  (let [lines (str/split-lines text)]
    (loop [blocks [] current [] lines lines]
      (if (empty? lines)
        (if (seq current) (conj blocks (vec current)) blocks)
        (let [line (first lines)
              bq?  (str/starts-with? (str/trim line) ">")]
          (if bq?
            (recur blocks (conj current line) (rest lines))
            (recur (if (seq current) (conj blocks (vec current)) blocks)
                   []
                   (rest lines))))))))

(defn ^:private parse-size-type-alignment [line]
  (when-let [m (re-find #"\*(.+?),\s*(.+?)\*" line)]
    (let [[_ size-type alignment] m
          parts (str/split (str/trim size-type) #"\s+" 2)
          size  (when (seq parts) (get size-codes (str/lower-case (first parts))))
          type  (when (> (count parts) 1) (str/lower-case (second parts)))
          align (mapv #(get alignment-codes (str/lower-case %))
                      (str/split (str/trim alignment) #"\s+"))]
      {:size (when size [size])
       :type type
       :alignment (vec (remove nil? align))})))

(defn ^:private parse-hp [value]
  (when-let [m (re-find #"(\d+)\s*\(([^)]+)\)" value)]
    {:average (js/parseInt (nth m 1))
     :formula (str/trim (nth m 2))}))

(defn ^:private parse-speed [value]
  (let [parts (str/split value #",\s*")]
    (into {}
          (keep
           (fn [part]
             (cond
               (re-find #"^(\d+)\s*ft\.?$" (str/trim part))
               (let [[_ n] (re-find #"^(\d+)\s*ft\.?$" (str/trim part))]
                 [:walk (js/parseInt n)])

               (re-find #"^(\w+)\s+(\d+)\s*ft\.?$" (str/trim part))
               (let [[_ mode n] (re-find #"^(\w+)\s+(\d+)\s*ft\.?$" (str/trim part))]
                 [(keyword (str/lower-case mode)) (js/parseInt n)])

               :else nil))
          parts))))

(defn ^:private parse-skills [value]
  (into {}
        (for [skill (str/split value #",\s*")
              :let [m (re-find #"^(\w+)\s+([+-]\d+)$" (str/trim skill))]
              :when m]
          [(keyword (str/lower-case (nth m 1))) (nth m 2)])))

(defn ^:private parse-list [value]
  (when (string? value)
    (vec (map str/trim (str/split value #",\s*")))))

(defn ^:private parse-ac [value]
  (when-let [n (re-find #"(\d+)" value)]
    [(js/parseInt (second n))]))

(defn ^:private parse-cr [value]
  (when-let [m (re-find #"^([\d/]+)" (str/trim value))]
    (nth m 1)))

(defn ^:private parse-passive [value]
  (when-let [m (re-find #"Passive Perception\s+(\d+)" value)]
    (js/parseInt (second m))))

(defn ^:private parse-ability-scores [lines]
  (let [table-lines (filter #(re-find #"\|\s*\d+" %) lines)]
    (when (seq table-lines)
      (let [row (first (filter #(re-find #"\d+\s*\([+-]?\d+\)" %) table-lines))
            scores (when row (re-seq #"(\d+)\s*\([+-]?\d+\)" row))]
        (when (= (count scores) 6)
          (zipmap [:str :dex :con :int :wis :cha]
                  (map #(js/parseInt (second %)) scores)))))))

(defn ^:private parse-named-entries [lines]
  (into []
        (keep
         (fn [line]
           (when-let [m (re-find #"\*\*\*(.+?)\.\*\*\*\s*(.*)" line)]
             {:name (str/trim (nth m 1))
              :entries [(str/trim (nth m 2))]}))
         lines)))

(defn ^:private parse-stat-line [line]
  (when-let [m (re-find #"-\s*\*\*(.+?)\*\*\s+(.+)" line)]
    (let [key (str/lower-case (str/replace (nth m 1) #" " ""))
          val (str/trim (nth m 2))]
      (case key
        "armorclass" [:ac (parse-ac val)]
        "hitpoints"  [:hp (parse-hp val)]
        "speed"      [:speed (parse-speed val)]
        "initiative" [:initiative {:bonus (first (re-seq #"[+-]\d+" val))}]
        "skills"     [:skill (parse-skills val)]
        "immunities" [:immune (mapv str/lower-case (parse-list val))]
        "resistances" [:resist (mapv str/lower-case (parse-list val))]
        "vulnerabilities" [:vulnerable (mapv str/lower-case (parse-list val))]
        "senses"     [:senses (parse-list val)]
        "languages"  [:languages (parse-list val)]
        "challenge"  [:cr (parse-cr val)]
        "proficiencybonus" [:proficiency-bonus val]
        "habitat"    [:environment (mapv str/lower-case (parse-list val))]
        "treasure"   [:treasure (mapv str/lower-case (parse-list val))]
        nil))))

(defn ^:private parse-block [block-lines]
  (let [lines (mapv strip-blockquote block-lines)
        name  (some (fn [l] (when-let [m (re-find #"^##\s+(.+)" l)] (str/trim (second m)))) lines)
        subtitle (some #(when (str/starts-with? % "*") %) lines)
        meta  (when subtitle (parse-size-type-alignment subtitle))
        stats (into {} (keep parse-stat-line) lines)
        abilities (parse-ability-scores lines)
        action-start (first (keep-indexed #(when (re-find #"^###\s+Actions" %2) %1) lines))
        action-lines (if action-start (subvec lines (inc action-start)) [])
        legendary-start (first (keep-indexed #(when (re-find #"^###\s+Legendary Actions" %2) %1) action-lines))
        action-only-lines (if legendary-start
                            (subvec action-lines 0 legendary-start)
                            action-lines)
        legendary-lines (if legendary-start
                          (subvec action-lines (inc legendary-start))
                          [])
        trait-entries (parse-named-entries (if action-start (subvec lines 0 action-start) lines))
        action-entries (parse-named-entries action-only-lines)
        legendary-entries (parse-named-entries legendary-lines)
        passive (some parse-passive (:senses stats))]
    (when name
      (cond-> {:name name}
        meta (merge (select-keys meta [:size :type :alignment]))
        (:ac stats) (assoc :ac (:ac stats))
        (:hp stats) (assoc :hp (:hp stats))
        (:speed stats) (assoc :speed (:speed stats))
        (:initiative stats) (assoc :initiative (:initiative stats))
        abilities (merge abilities)
        (:skill stats) (assoc :skill (:skill stats))
        (:senses stats) (assoc :senses (:senses stats))
        passive (assoc :passive passive)
        (:immune stats) (assoc :immune (:immune stats))
        (:resist stats) (assoc :resist (:resist stats))
        (:vulnerable stats) (assoc :vulnerable (:vulnerable stats))
        (:languages stats) (assoc :languages (:languages stats))
        (:cr stats) (assoc :cr (:cr stats))
        (:environment stats) (assoc :environment (:environment stats))
        (:treasure stats) (assoc :treasure (:treasure stats))
        (seq trait-entries) (assoc :trait trait-entries)
        (seq action-entries) (assoc :action action-entries)
        (seq legendary-entries) (assoc :legendary legendary-entries)
        true (assoc :hasToken true :hasFluff true)))))

(defn validate-sheet [sheet]
  (let [errors (cond-> []
                 (not (string? (:name sheet))) (conj "name must be a string")
                 (not (seq (:name sheet))) (conj "name is required")
                 (and (contains? sheet :ac) (not (vector? (:ac sheet)))) (conj "ac must be a vector")
                 (and (contains? sheet :hp)
                      (not (and (map? (:hp sheet))
                                (number? (:average (:hp sheet)))
                                (string? (:formula (:hp sheet))))))
                 (conj "hp must have average and formula")
                 (and (contains? sheet :str) (not (number? (:str sheet)))) (conj "str must be a number"))]
    (if (seq errors)
      {:valid? false :errors errors :sheet nil}
      {:valid? true :errors [] :sheet sheet})))

(defn ^:private normalize-text [text]
  (-> text
      (str/replace #"\r\n" "\n")
      (str/replace #"\r" "\n")))

(defn ^:private normalize-pdf-text [text]
  (-> text
      normalize-text
      (str/replace #"\n[ \t]*\n+" "\n")
      (str/replace #"[ \t]*\n[ \t]*" "\n")
      str/trim))

(defn parse-markdown [text]
  (let [text (normalize-text text)
        blocks (extract-stat-blocks text)
        sheets (into [] (keep parse-block) blocks)]
    (if (seq sheets)
      {:valid? true :sheets sheets :errors []}
      {:valid? false :sheets [] :errors ["No stat blocks found in markdown"]})))

(defn ^:private roll20-line [line]
  (str/trim (cond
               (string? line) line
               (nil? line) ""
               :else (str line))))

(defn ^:private roll20-re-find [pattern s]
  (re-find pattern (if (string? s) s (str (or s "")))))

(def ^:private roll20-ability-labels
  {"STRENGTH" :str
   "DEXTERITY" :dex
   "CONSTITUTION" :con
   "INTELLIGENCE" :int
   "WISDOM" :wis
   "CHARISMA" :cha})

(defn ^:private roll20-name [text]
  (or (some-> (re-find #"(?is)^\s*([^\n]+)\s*\n\s*NAME\s*\n\s*[^\n]+\s*\n\s*BACKGROUND" text)
              second
              str/trim)
      (some-> (re-find #"(?is)^\s*([^\n]+)\s*\n\s*NAME\b" text)
              second
              str/trim)))

(defn ^:private roll20-ac [text]
  (when-let [m (re-find #"(?i)ARMOR\s*\nCLASS\s*\n(\d+)" text)]
    [(js/parseInt (second m))]))

(defn ^:private roll20-hit-dice [text]
  (when-let [m (re-find #"(?i)HIT DICE\s*\nSPENT\s*\n(\d+[dD]\d+)" text)]
    (str/lower-case (second m))))

(defn ^:private roll20-hp [text]
  (when-let [m (re-find #"(?i)HIT POINTS\s*\n(?:TEMP\s*\n)?(\d+)\s*\nCURRENT\s*\n(\d+)\s*\nMAX" text)]
    (let [max-hp (js/parseInt (nth m 2))]
      {:average max-hp
       :formula (or (roll20-hit-dice text) (str max-hp " max"))})))

(defn ^:private roll20-initiative [text]
  (when-let [m (re-find #"(?i)INITIATIVE\s*\n([+-]?\d+)" text)]
    (let [bonus (roll20-line (second m))]
      (when (seq bonus)
        {:bonus (if (roll20-re-find #"^[+-]" bonus) bonus (str "+" bonus))}))))

(defn ^:private roll20-speed [text]
  (when-let [m (re-find #"(?i)SPEED\s*\n(\d+)\s*ft" text)]
    {:walk (js/parseInt (second m))}))

(defn ^:private roll20-abilities [text]
  (into {}
        (keep
         (fn [[label kw]]
           (when-let [m (re-find (re-pattern (str "(?i)" label
                                                  "\\s*\\n[+-]?\\d+\\s*\\nMODIFIER\\s*\\n(\\d+)\\s*\\nSCORE"))
                                  text)]
             [kw (js/parseInt (second m))]))
         roll20-ability-labels)))

(defn ^:private roll20-alignment [text]
  (when-let [m (re-find #"(?i)ALIGNMENT\s+(.+?)(?:\s+PRONOUNS|\n)" text)]
    (let [parts (str/split (str/trim (second m)) #"\s+")]
      (vec (remove nil? (mapv #(get alignment-codes (str/lower-case %)) parts))))))

(defn ^:private roll20-class [text]
  (some-> (re-find #"(?i)([^\n]+)\nCLASS\b" text) second str/trim))

(defn ^:private roll20-species [text]
  (when-let [m (re-find #"(?is)BACKGROUND\s*\n\s*[^\n]+\s*\n\s*CLASS\s*\n\s*([^\n]+(?:\s*\n\s*[^\n]+)*?)\s*\n\s*SPECIES" text)]
    (-> (second m)
        str
        (str/replace #"\s*\n\s*" " ")
        str/trim)))

(defn ^:private roll20-passive [text]
  (when-let [m (re-find #"(?i)PASSIVE PERCEPTION\s*\n(\d+)" text)]
    (js/parseInt (second m))))

(defn ^:private roll20-languages [text]
  (when-let [m (re-find #"(?i)LANGUAGES\s*\n([\s\S]*?)(?=\n(?:RESISTANCES|ALIGNMENT|CLASS|SPECIES|EQUIPMENT)|$)" text)]
    (let [body (str (or (second m) ""))]
      (when (seq (str/trim body))
        (parse-list (str/replace body #"\n" ", "))))))

(defn ^:private roll20-resistances [text]
  (when-let [m (re-find #"(?i)RESISTANCES\s+([^\n]+)" text)]
    (let [value (roll20-line (second m))]
      (when (seq value)
        (when-not (roll20-re-find #"(?i)^none$" value)
          (mapv str/lower-case (parse-list value)))))))

(defn ^:private roll20-section-body [text header end-pattern]
  (when-let [m (re-find (re-pattern (str "(?is)" header "\\s*\\nNAME[^\n]*\\n([\\s\\S]*?)(?=\\n(?:"
                                            end-pattern
                                            ")|$)"))
                        text)]
    (vec (str/split-lines (str (or (second m) ""))))))

(defn ^:private roll20-table-skip? [line]
  (let [s (roll20-line line)]
    (or (str/blank? s)
        (roll20-re-find #"(?i)^(NAME|ATK|DC|DAMAGE|RANGE|NOTES|/)$" s))))

(defn ^:private roll20-excluded-title? [line]
  (roll20-re-find #"(?i)^(actions in combat|reactions in combat|opportunity attack|channel divinity|actions|bonus actions|reactions|free actions|spells & spellcasting)$"
                  (roll20-line line)))

(defn ^:private roll20-em-dash? [line]
  (contains? #{"—" "-" "–"} (roll20-line line)))

(defn ^:private roll20-range? [line]
  (roll20-re-find #"(?i)^(self|touch|\d+\s*(?:ft\.?|feet))$" (roll20-line line)))

(defn ^:private roll20-attack-name? [line]
  (let [s (roll20-line line)]
    (and (seq s)
         (< (count s) 40)
         (roll20-re-find #"^[A-Za-z]" s)
         (not (roll20-re-find #"[a-z]" s))
         (not (roll20-re-find #"," s))
         (not (roll20-re-find #"(?i)^(once |you |as |the |when |if |on |instead |each |a target)" s))
         (not (roll20-re-find #"(?i)\b(of|the|that|when|each|instead|you|your|creature|damage|equal|none|feet|wide|line|butt)\b" s))
         (not (roll20-re-find #"(?i)^DC\s+\d+$" s))
         (not (roll20-table-skip? s))
         (not (roll20-excluded-title? s))
         (not (roll20-range? s))
         (not (roll20-re-find #"^[+-]\d+$" s))
         (not (roll20-re-find #"(?i)^(WIS|DEX|STR|INT|CON|CHA)\s+\d+$" s)))))

(defn ^:private roll20-damage-line? [line]
  (roll20-re-find #"(?i)\d+d\d+|[+-]\d+|bludgeon|pierc|slash|radiant|fire|psychic|force|cold|necrot"
                  (roll20-line line)))

(defn ^:private roll20-collect-attack-fields
  [lines start]
  (loop [i start
         mods []
         dmg []
         range nil
         in-dmg? false]
    (if (>= i (count lines))
      {:mods mods :damage (str/trim (str/join " " dmg)) :range range :next i}
      (let [s (roll20-line (nth lines i))]
        (cond
          (and (roll20-attack-name? s)
               (or (seq mods) (seq dmg) range))
          {:mods mods :damage (str/trim (str/join " " dmg)) :range range :next i}

          (roll20-table-skip? s)
          (recur (inc i) mods dmg range in-dmg?)

          (roll20-range? s)
          (recur (inc i) mods dmg s in-dmg?)

          (roll20-em-dash? s)
          (recur (inc i) (if in-dmg? mods (conj mods "—")) dmg range in-dmg?)

          (and (= s "+") (< (inc i) (count lines))
               (roll20-re-find #"^\d+$" (roll20-line (nth lines (inc i)))))
          (recur (+ i 2)
                 (conj mods (str "+" (roll20-line (nth lines (inc i)))))
                 dmg range in-dmg?)

          (and (= s "+") in-dmg?)
          (recur (inc i) mods (conj dmg s) range true)

          (= s "+")
          (recur (inc i) mods dmg range in-dmg?)

          (roll20-re-find #"^[+-]\d+$" s)
          (recur (inc i) (conj mods s) dmg range in-dmg?)

          (roll20-re-find #"(?i)^(WIS|DEX|STR|INT|CON|CHA)\s+\d+$" s)
          (recur (inc i) (conj mods (str/upper-case s)) dmg range in-dmg?)

          (roll20-damage-line? s)
          (recur (inc i) mods (conj dmg s) range true)

          in-dmg?
          (recur (inc i) mods (conj dmg s) range true)

          :else
          (recur (inc i) mods dmg range in-dmg?))))))

(defn ^:private roll20-parse-inline-attack [line]
  (let [s (roll20-line line)]
    (cond
      (roll20-re-find #"^(.+?)\s+[+-]\d+\s+[+-]\d+\s+" s)
      (let [[_ name _ atk damage]
            (roll20-re-find #"^(.+?)\s+([+-]\d+)\s+([+-]\d+)\s+(.+)$" s)]
        (when name
          {:name (roll20-line name) :atk atk :damage (roll20-line damage)}))

      (roll20-re-find #"^(.+?)\s+[+-]\d+\s+(\d+d\d+|[+-]\d+)" s)
      (let [[_ name atk damage]
            (roll20-re-find #"^(.+?)\s+([+-]\d+)\s+(.+)$" s)]
        (when name
          {:name (roll20-line name) :atk atk :damage (roll20-line damage)}))

      :else nil)))

(defn ^:private roll20-pick-atk [mods]
  (let [mods (filter string? mods)]
    (or (some #(roll20-re-find #"(?i)^(WIS|DEX|STR|INT|CON|CHA)\s+\d+$" %) mods)
        (last (filter #(roll20-re-find #"^\+[0-9]+$" %) mods))
        (last mods))))

(defn ^:private roll20-format-attack-entry
  [atk damage range]
  (let [atk (when atk (roll20-line atk))]
    (cond
      (and atk (roll20-re-find #"(?i)^(WIS|DEX|STR|INT|CON|CHA)\s+(\d+)$" atk))
      (let [[_ ability dc] (roll20-re-find #"(?i)^(WIS|DEX|STR|INT|CON|CHA)\s+(\d+)$" atk)
            ability-name (case (str/lower-case ability)
                             "wis" "Wisdom"
                             "dex" "Dexterity"
                             "str" "Strength"
                             "int" "Intelligence"
                             "con" "Constitution"
                             "cha" "Charisma"
                             ability)]
        (str ability-name " Saving Throw: DC " dc
             (when (seq damage) (str ". Damage: " damage))
             (when range (str ". Range: " range))))

      (and atk (roll20-re-find #"^\+[0-9]+$" atk))
      (str "Melee Attack Roll: " atk ". Hit: " (if (seq damage) damage "—")
           (when range (str ". Range: " range)))

      (seq damage)
      (str "Hit: " damage (when range (str ". Range: " range)))

      :else nil)))

(defn ^:private roll20-parse-attack-table [lines]
  (loop [i 0 entries []]
    (if (>= i (count lines))
      entries
      (let [line (nth lines i)]
        (if (or (roll20-table-skip? line) (roll20-excluded-title? line))
          (recur (inc i) entries)
          (if-some [inline (roll20-parse-inline-attack line)]
            (let [{:keys [name atk damage]} inline
                  entry (roll20-format-attack-entry atk damage nil)]
              (recur (inc i)
                     (if entry
                       (conj entries {:name name :entries [entry]})
                       entries)))
            (if (roll20-attack-name? line)
              (let [name (roll20-line line)
                    {:keys [mods damage range next]}
                    (roll20-collect-attack-fields lines (inc i))
                    atk (roll20-pick-atk mods)
                    entry (roll20-format-attack-entry atk damage range)]
                (recur next
                       (if entry
                         (conj entries {:name name :entries [entry]})
                         entries)))
              (recur (inc i) entries))))))))

(defn ^:private roll20-note-title? [line]
  (let [s (roll20-line line)]
    (and (seq s)
         (< (count s) 72)
         (roll20-re-find #"^[A-Z]" s)
         (not (roll20-table-skip? s))
         (not (roll20-excluded-title? s))
         (not (roll20-range? s))
         (not (roll20-re-find #"^[+-]\d+$" s))
         (not (roll20-re-find #"(?i)^(once |you |as |the |when |if |on |instead |each |a target)" s)))))

(defn ^:private roll20-collect-notes [lines start]
  (loop [i start parts []]
    (if (>= i (count lines))
      {:notes (str/trim (str/join " " parts)) :next i}
      (let [s (roll20-line (nth lines i))]
        (if (and (seq parts) (roll20-note-title? s))
          {:notes (str/trim (str/join " " parts)) :next i}
          (recur (inc i) (conj parts s)))))))

(defn ^:private roll20-parse-name-notes [lines]
  (loop [i 0 entries []]
    (if (>= i (count lines))
      entries
      (let [line (roll20-line (nth lines i))]
        (if (or (roll20-table-skip? line) (roll20-excluded-title? line))
          (recur (inc i) entries)
          (if (roll20-note-title? line)
            (let [{:keys [notes next]} (roll20-collect-notes lines (inc i))]
              (if (seq notes)
                (recur next (conj entries {:name line :entries [notes]}))
                (recur next entries)))
            (recur (inc i) entries)))))))

(defn ^:private roll20-dedupe-actions [entries]
  (vals (reduce (fn [acc {:keys [name] :as entry}]
                  (let [key (str/lower-case (roll20-line name))]
                    (if (contains? acc key)
                      (let [existing (get acc key)]
                        (if (> (count (str/join " " (:entries entry)))
                               (count (str/join " " (:entries existing))))
                          (assoc acc key entry)
                          acc))
                      (assoc acc key entry))))
                {}
                entries)))

(defn ^:private roll20-actions [text]
  (let [attacks (roll20-section-body text "ATTACKS"
                                     "WEAPON MASTERIES|ACTIONS|BONUS ACTIONS|DEFENSES|COMBAT REFERENCE")
        weapons (roll20-section-body text "WEAPONS & DAMAGE CANTRIPS"
                                       "CLASS FEATURES|SPECIES TRAITS|FEATS|CHA\\s")
        actions (roll20-section-body text "ACTIONS"
                                     "BONUS ACTIONS|REACTIONS|FREE ACTIONS|SPELLS")
        bonus   (roll20-section-body text "BONUS ACTIONS"
                                     "REACTIONS|FREE ACTIONS|SPELLS")
        reactions (roll20-section-body text "REACTIONS"
                                       "FREE ACTIONS|SPELLS")
        entries (roll20-dedupe-actions
                 (into []
                       (concat (roll20-parse-attack-table (or attacks []))
                               (roll20-parse-name-notes (or actions []))
                               (map #(update % :name (fn [n] (str n " (Bonus Action)"))) 
                                    (roll20-parse-name-notes (or bonus [])))
                               (map #(update % :name (fn [n] (str n " (Reaction)"))) 
                                    (roll20-parse-name-notes (or reactions [])))
                               (roll20-parse-attack-table (or weapons [])))))]
    (vec (remove #(str/blank? (str/join " " (:entries %))) entries))))

(def ^:private roll20-stat-long-labels
  #{"hit points" "speed" "saving throws" "skills"
    "damage resistances" "damage immunities" "condition immunities"
    "senses" "languages" "challenge" "proficiency bonus" "armor class"
    "actions" "bonus actions" "reactions" "legendary actions"})

(defn ^:private roll20-stat-ability-line? [s]
  (contains? #{"STR" "DEX" "CON" "INT" "WIS" "CHA"} (str/upper-case s)))

(defn ^:private roll20-stat-label? [line]
  (let [s (roll20-line line)]
    (or (contains? roll20-stat-long-labels (str/lower-case s))
        (roll20-stat-ability-line? s))))

(defn ^:private roll20-stat-raw-join [parts]
  (->> parts
       (remove #(= % ","))
       (map roll20-line)
       (str/join " ")
       (#(-> % (str/replace #"\s{2,}" " ") str/trim))))

(defn ^:private roll20-stat-normalize-plus-paren [s]
  (if-let [m (re-find #"\(\s*\+\s*(\d+)\s*\)" s)]
    (str/replace s (first m) (str "(+" (second m) ")"))
    s))

(defn ^:private roll20-stat-join-parts [parts]
  (when (seq parts)
    (-> parts
        roll20-stat-raw-join
        roll20-stat-normalize-plus-paren
        (str/replace #"\s*,\s*" ", ")
        str/trim)))

(defn ^:private roll20-stat-join-prose [parts]
  (when (seq parts)
    (-> (str/join " " (remove #(= % ".") parts))
        (str/replace #"\s{2,}" " ")
        str/trim)))

(defn ^:private parse-roll20-stat-subtitle [line]
  (cond
    (re-find #"^\*(.+?),\s*(.+?)\*$" line)
    (parse-size-type-alignment line)

    (re-find #"(?i)^(\w+)\s+([^(,]+?)\s*\(([^)]+)\),\s*(.+)$" line)
    (let [[_ size type-part subtype alignment]
          (re-find #"(?i)^(\w+)\s+([^(,]+?)\s*\(([^)]+)\),\s*(.+)$" line)]
      {:size (when-let [s (get size-codes (str/lower-case (roll20-line size)))] [s])
       :type (some-> type-part roll20-line str/lower-case)
       :subtype (some-> subtype roll20-line str/lower-case)
       :alignment (vec (remove nil?
                               (mapv #(get alignment-codes (str/lower-case %))
                                     (str/split (roll20-line alignment) #"\s+"))))})

    (re-find #"(?i)^(\w+)\s+([^,]+),\s*(.+)$" line)
    (let [[_ size type alignment] (re-find #"(?i)^(\w+)\s+([^,]+),\s*(.+)$" line)]
      {:size (when-let [s (get size-codes (str/lower-case (roll20-line size)))] [s])
       :type (some-> type roll20-line str/lower-case)
       :alignment (vec (remove nil?
                               (mapv #(get alignment-codes (str/lower-case %))
                                     (str/split (roll20-line alignment) #"\s+"))))})

    :else nil))

(defn ^:private roll20-stat-entry-title? [line]
  (let [s (roll20-line line)]
    (and (seq s)
         (< (count s) 72)
         (not (roll20-stat-label? s))
         (re-find #"^[A-Z]" s)
         (not (re-find #"[.!?]$" s))
         (not= s ".")
         (not (str/ends-with? s ":"))
         (not (roll20-re-find #"(?i)^(Melee|Weapon|Attack|Hit:?|Ranged)$" s))
         (not (roll20-re-find #"(?i)^(Melee|Ranged)\s+Weapon\s+Attack:?$" s)))))

(defn ^:private roll20-stat-normalize-action-text [text]
  (let [text (roll20-line text)]
    (when (seq text)
      (-> text
          (str/replace #"(?i)Melee Weapon Attack:\s*([+-]\d+)\s+to hit"
                       (fn [_match n] (str "*Melee Attack Roll:* " (roll20-line n))))
          (str/replace #"(?i)Ranged Weapon Attack:\s*([+-]\d+)\s+to hit"
                       (fn [_match n] (str "*Ranged Attack Roll:* " (roll20-line n))))
          (str/replace #"(?i)\bHit:\s*" "*Hit:* ")))))

(defn ^:private roll20-stat-collect-entry [lines start]
  (loop [i start parts []]
    (if (>= i (count lines))
      {:notes (roll20-stat-join-prose parts) :next i}
      (let [s (roll20-line (nth lines i))]
        (cond
          (and (empty? parts) (= s "."))
          (recur (inc i) parts)

          (and (seq parts) (roll20-stat-entry-title? s))
          {:notes (roll20-stat-join-prose parts) :next i}

          :else
          (recur (inc i) (conj parts s)))))))

(defn ^:private roll20-stat-parse-entries [lines]
  (loop [i 0 entries []]
    (if (>= i (count lines))
      entries
      (let [line (roll20-line (nth lines i))]
        (if (roll20-stat-entry-title? line)
          (let [{:keys [notes next]} (roll20-stat-collect-entry lines (inc i))]
            (if-let [entry (roll20-stat-normalize-action-text notes)]
              (recur next (conj entries {:name line :entries [entry]}))
              (recur next entries)))
          (recur (inc i) entries))))))

(defn ^:private roll20-stat-ability-complete? [joined]
  (re-find #"\d+\s*\(\s*\+?\s*\d+\s*\)" joined))

(defn ^:private roll20-stat-collect-ability [lines start]
  (loop [i start parts []]
    (if (>= i (count lines))
      {:value (roll20-stat-join-parts parts) :next i}
      (let [s (roll20-line (nth lines i))]
        (if (roll20-stat-label? s)
          {:value (roll20-stat-join-parts parts) :next i}
          (let [parts' (conj parts s)
                raw (roll20-stat-raw-join parts')
                joined (roll20-stat-join-parts parts')]
            (if (roll20-stat-ability-complete? raw)
              {:value joined :next (inc i)}
              (recur (inc i) parts'))))))))

(defn ^:private roll20-stat-collect-hp [lines start]
  (loop [i start parts []]
    (if (>= i (count lines))
      {:value (roll20-stat-join-parts parts) :next i}
      (let [s (roll20-line (nth lines i))]
        (if (roll20-stat-label? s)
          {:value (roll20-stat-join-parts parts) :next i}
          (let [parts' (conj parts s)
                joined (roll20-stat-join-parts parts')]
            (if (re-find #"\d+\s*\(\s*[^)]+\s*\)" joined)
              {:value joined :next (inc i)}
              (recur (inc i) parts'))))))))

(defn ^:private roll20-stat-collect-until-label [lines start]
  (loop [i start parts []]
    (if (>= i (count lines))
      {:value (roll20-stat-join-parts parts) :next i}
      (let [s (roll20-line (nth lines i))]
        (if (roll20-stat-label? s)
          {:value (roll20-stat-join-parts parts) :next i}
          (recur (inc i) (conj parts s)))))))

(def ^:private roll20-stat-ability-labels
  #{"str" "dex" "con" "int" "wis" "cha"})

(def ^:private roll20-stat-section-labels
  #{"actions" "bonus actions" "reactions" "legendary actions"})

(defn ^:private roll20-stat-collect-field [lines start label]
  (cond
    (contains? roll20-stat-ability-labels label)
    (roll20-stat-collect-ability lines start)

    (= label "hit points")
    (roll20-stat-collect-hp lines start)

    (contains? roll20-stat-section-labels label)
    {:value nil :next start}

    :else
    (roll20-stat-collect-until-label lines start)))

(defn ^:private roll20-stat-walk [lines]
  (loop [i 0
         fields {}
         cha-end nil
         actions-idx nil]
    (if (>= i (count lines))
      {:fields fields :cha-end cha-end :actions-idx actions-idx}
      (let [s (roll20-line (nth lines i))]
        (if (roll20-stat-label? s)
          (let [label (str/lower-case s)]
            (cond
              (= label "actions")
              (recur (inc i) fields cha-end i)

              (= label "cha")
              (let [{:keys [value next]} (roll20-stat-collect-field lines (inc i) label)]
                (recur next (if value (assoc fields label value) fields) next actions-idx))

              (#{"str" "dex" "con" "int" "wis"} label)
              (let [{:keys [value next]} (roll20-stat-collect-field lines (inc i) label)]
                (recur next (if value (assoc fields label value) fields) cha-end actions-idx))

              :else
              (let [{:keys [value next]} (roll20-stat-collect-field lines (inc i) label)]
                (recur next (if value (assoc fields label value) fields) cha-end actions-idx))))
          (recur (inc i) fields cha-end actions-idx))))))

(defn ^:private roll20-stat-label-indices [lines]
  (vec (keep-indexed #(when (roll20-stat-label? %2) %1) lines)))

(defn ^:private roll20-stat-extract-fields [lines]
  (:fields (roll20-stat-walk lines)))

(defn ^:private roll20-stat-parse-ability [value]
  (let [value (roll20-line value)]
    (when (seq value)
      (or (when-let [m (re-find #"(\d+)\s*\(\s*\+?\s*\d+\s*\)" value)]
            (js/parseInt (second m)))
          (when-let [m (re-find #"^(\d+)" value)]
            (js/parseInt (second m)))))))

(defn ^:private roll20-stat-parse-hp [value]
  (let [value (roll20-line value)]
    (when (seq value)
      (or (parse-hp value)
          (when-let [avg (re-find #"(\d+)" value)]
            (when-let [formula (re-find #"(?i)(\d+d\d+[^)\s]*)" value)]
              {:average (js/parseInt (second avg))
               :formula (roll20-line (second formula))}))))))

(defn ^:private roll20-stat-passive [value]
  (when-let [m (re-find #"(?i)passive Perception\s+(\d+)" (str value))]
    (js/parseInt (second m))))

(defn ^:private roll20-stat-block? [text]
  (and (not (roll20-sheet? text))
       (re-find #"(?i)Armor Class\s*\n" text)
       (re-find #"(?i)Hit Points\s*\n" text)
       (re-find #"(?is)STR\s*\n\s*\d+" text)
       (re-find #"(?i)\bActions\b" text)))

(defn ^:private parse-roll20-stat-block [text]
  (let [lines (vec (str/split-lines text))
        name (roll20-line (first lines))
        subtitle (when (> (count lines) 1) (roll20-line (nth lines 1)))
        meta (when subtitle (parse-roll20-stat-subtitle subtitle))
        {:keys [fields cha-end actions-idx]} (roll20-stat-walk lines)
        trait-lines (when (and actions-idx cha-end (< cha-end actions-idx))
                      (subvec lines cha-end actions-idx))
        action-lines (when actions-idx (subvec lines (inc actions-idx)))
        trait-entries (when trait-lines (roll20-stat-parse-entries trait-lines))
        action-entries (when action-lines (roll20-stat-parse-entries action-lines))
        passive (roll20-stat-passive (get fields "senses"))]
    (when (seq name)
      (cond-> {:name name}
        meta (merge (select-keys meta [:size :type :subtype :alignment]))
        (some-> (get fields "armor class") parse-ac)
        (assoc :ac (some-> (get fields "armor class") parse-ac))
        (some-> (get fields "hit points") roll20-stat-parse-hp)
        (assoc :hp (some-> (get fields "hit points") roll20-stat-parse-hp))
        (get fields "speed") (assoc :speed (parse-speed (get fields "speed")))
        (some-> (get fields "str") roll20-stat-parse-ability)
        (assoc :str (some-> (get fields "str") roll20-stat-parse-ability))
        (some-> (get fields "dex") roll20-stat-parse-ability)
        (assoc :dex (some-> (get fields "dex") roll20-stat-parse-ability))
        (some-> (get fields "con") roll20-stat-parse-ability)
        (assoc :con (some-> (get fields "con") roll20-stat-parse-ability))
        (some-> (get fields "int") roll20-stat-parse-ability)
        (assoc :int (some-> (get fields "int") roll20-stat-parse-ability))
        (some-> (get fields "wis") roll20-stat-parse-ability)
        (assoc :wis (some-> (get fields "wis") roll20-stat-parse-ability))
        (some-> (get fields "cha") roll20-stat-parse-ability)
        (assoc :cha (some-> (get fields "cha") roll20-stat-parse-ability))
        (get fields "skills") (assoc :skill (parse-skills (get fields "skills")))
        (get fields "damage resistances")
        (assoc :resist (mapv str/lower-case (or (parse-list (get fields "damage resistances")) [])))
        (get fields "damage immunities")
        (assoc :immune (mapv str/lower-case (or (parse-list (get fields "damage immunities")) [])))
        (get fields "senses") (assoc :senses (or (parse-list (get fields "senses")) []))
        passive (assoc :passive passive)
        (get fields "languages") (assoc :languages (or (parse-list (get fields "languages")) []))
        (get fields "challenge") (assoc :cr (parse-cr (get fields "challenge")))
        (get fields "proficiency bonus")
        (assoc :proficiency-bonus (get fields "proficiency bonus"))
        (seq trait-entries) (assoc :trait trait-entries)
        (seq action-entries) (assoc :action action-entries)
        true (assoc :hasToken true :hasFluff true)))))

(def ^:private visual-class-names
  #{"barbarian" "bard" "cleric" "druid" "fighter" "monk"
    "paladin" "ranger" "rogue" "sorcerer" "warlock" "wizard"})

(def ^:private visual-species-names
  #{"human" "elf" "dwarf" "halfling" "dragonborn" "gnome"
    "half-elf" "half-orc" "tiefling" "orc" "goliath" "aasimar"})

(defn ^:private visual-sheet-lines [text]
  (mapv roll20-line (str/split-lines (normalize-text text))))

(defn ^:private visual-character-sheet? [text]
  (and (not (roll20-sheet? text))
       (not (roll20-stat-block? text))
       (re-find #"\"[^\"]+\"\s*-\s*.+" text)
       (re-find #"(?i)\d+\s*ft" text)
       (re-find #"\d+d\d+" text)
       (some #(re-find (re-pattern (str "(?i)\\b" % "\\b")) text)
             visual-class-names)))

(defn ^:private visual-sheet-name [lines]
  (when-let [line (first lines)]
    (or (some-> (re-find #"\"([^\"]+)\"" line) second str/trim)
        (str/trim line))))

(defn ^:private visual-sheet-hp [lines]
  (some (fn [i]
          (when (< (inc i) (count lines))
            (let [a (nth lines i)
                  b (nth lines (inc i))]
              (when (and (re-find #"^\d+$" a)
                         (re-find #"^\d+d\d+" b))
                {:average (js/parseInt a) :formula b}))))
        (range (count lines))))

(defn ^:private visual-sheet-speed [lines]
  (when-let [line (some #(re-find #"^(\d+)\s*ft" %) lines)]
    {:walk (js/parseInt (second line))}))

(defn ^:private visual-sheet-ac [lines]
  (when-let [height-idx (first (keep-indexed #(when (re-find #"^\d+'\d+\"" %2) %1) lines))]
    (when (< (inc height-idx) (count lines))
      (let [candidate (nth lines (inc height-idx))]
        (when (re-find #"^\d{1,2}$" candidate)
          [(js/parseInt candidate)])))))

(defn ^:private visual-sheet-proficiency [lines]
  (some #(when-let [m (re-find #"^\+(\d+)$" %)]
           (str "+" (second m)))
        lines))

(defn ^:private visual-sheet-level [lines]
  (some (fn [line]
          (when-let [m (re-find #"^(\d{1,2})$" line)]
            (let [n (js/parseInt (second m))]
              (when (and (>= n 1) (<= n 20)) (str n)))))
        (take 12 lines)))

(defn ^:private visual-sheet-class [lines]
  (some #(when (contains? visual-class-names (str/lower-case %))
           (str/lower-case %))
        lines))

(defn ^:private visual-sheet-species [lines]
  (some #(when (contains? visual-species-names (str/lower-case %))
           (str/lower-case %))
        lines))

(defn ^:private visual-sheet-subclass [lines]
  (some #(when (re-find #"(?i)^(College|School|Circle|Path|Domain) of\b" %)
           %)
        lines))

(defn ^:private visual-sheet-languages [lines]
  (some (fn [line]
          (when (re-find #"(?i)^Common," line)
            (parse-list line)))
        lines))

(defn ^:private visual-sheet-feature-line? [line]
  (let [s (roll20-line line)]
    (and (> (count s) 8)
         (re-find #"\s" s)
         (re-find #"^[A-Z]" s)
         (re-find #"[a-z]{3,}" s)
         (not (re-find #"^[+-]?\d+$" s))
         (not (re-find #"^\d+$" s))
         (not (re-find #"^\d+\s*ft" s))
         (not (re-find #"^\d+d\d+" s))
         (not (re-find #"^\d+'\d+\"" s))
         (not (contains? visual-class-names (str/lower-case s)))
         (not (contains? visual-species-names (str/lower-case s)))
         (not (re-find #"(?i)^(Forgery|Costume|Armour|Backpack|Lantern|Days|Waterskin)" s)))))

(defn ^:private visual-sheet-trait-entry [line]
  (if-let [[_ name body] (re-find #"^([^:]+):\s*(.+)$" line)]
    {:name (str/trim name) :entries [(str/trim body)]}
    {:name line :entries []}))

(defn ^:private visual-sheet-traits [lines]
  (let [start (or (first (keep-indexed #(when (re-find #"(?i)Bardic Inspiration|Expertise:|Jack of All Trades" %2) %1) lines))
                  0)
        end (or (first (keep-indexed #(when (= "Charisma" %2) %1) lines))
                (count lines))]
    (vec (for [line (subvec lines start end)
               :when (visual-sheet-feature-line? line)]
           (visual-sheet-trait-entry line)))))

(defn ^:private visual-sheet-spell-lines [lines]
  (let [start (or (first (keep-indexed #(when (= "Charisma" %2) %1) lines))
                  -1)
        end (or (first (keep-indexed #(when (re-find #"(?i)^Forgery Kit," %2) %1) lines))
                (count lines))]
    (when (pos? start)
      (vec (keep (fn [line]
                   (when (and (re-find #"^[A-Z]" line)
                              (not (re-find #"^\d+$" line))
                              (not (= line "Charisma"))
                              (not (re-find #"^\+(\d+)$" line))
                              (not (re-find #"^C$" line))
                              (> (count line) 3))
                     line))
                 (subvec lines (inc start) end))))))

(defn ^:private visual-sheet-abilities [lines ac]
  (let [ac-val (first ac)
        start (or (first (keep-indexed #(when (re-find #"^\d+'\d+\"" %2) %1) lines)) 0)
        end (or (first (keep-indexed #(when (visual-sheet-feature-line? %2) %1) lines))
                (count lines))
        scores (loop [i (+ start 2)
                      seen #{}
                      out []]
                 (if (or (>= i end) (>= (count out) 6))
                   out
                   (let [line (nth lines i)]
                     (if (and (re-find #"^\d{1,2}$" line)
                              (<= 6 (js/parseInt line) 20)
                              (not= (js/parseInt line) ac-val)
                              (not (contains? seen line)))
                       (recur (inc i) (conj seen line) (conj out (js/parseInt line)))
                       (recur (inc i) seen out)))))]
    (when (>= (count scores) 4)
      (cond-> {}
        (first scores) (assoc :str (nth scores 0))
        (> (count scores) 1) (assoc :dex (nth scores 1))
        (> (count scores) 2) (assoc :con (nth scores 2))
        (> (count scores) 3) (assoc :int (nth scores 3))
        (> (count scores) 4) (assoc :wis (nth scores 4))
        (> (count scores) 5) (assoc :cha (nth scores 5))))))

(defn ^:private parse-visual-character-sheet [text]
  (let [lines (visual-sheet-lines text)
        name (visual-sheet-name lines)
        ac (visual-sheet-ac lines)
        spells (visual-sheet-spell-lines lines)
        traits (into (visual-sheet-traits lines)
                     (when (seq spells)
                       [{:name "Spells"
                         :entries [(str/join ", " spells)]}]))]
    (when (seq name)
      (cond-> {:name name}
        (visual-sheet-hp lines) (assoc :hp (visual-sheet-hp lines))
        ac (assoc :ac ac)
        (visual-sheet-speed lines) (assoc :speed (visual-sheet-speed lines))
        (visual-sheet-proficiency lines)
        (assoc :proficiency-bonus (visual-sheet-proficiency lines))
        (visual-sheet-level lines) (assoc :cr (visual-sheet-level lines))
        (visual-sheet-class lines) (assoc :type "humanoid")
        (visual-sheet-species lines) (assoc :subtype (visual-sheet-species lines))
        (visual-sheet-species lines) (assoc :size ["M"])
        (visual-sheet-abilities lines ac) (merge (visual-sheet-abilities lines ac))
        (visual-sheet-languages lines) (assoc :languages (visual-sheet-languages lines))
        (seq traits) (assoc :trait traits)
        true (assoc :hasToken true :hasFluff true)))))

(defn ^:private parse-visual-character-sheet-text [text]
  (try
    (let [text (normalize-text text)]
      (if-not (visual-character-sheet? text)
        {:valid? false :sheets [] :errors ["No visual character sheet found in PDF"]}
        (if-let [sheet (parse-visual-character-sheet text)]
          {:valid? true :sheets [sheet] :errors []}
          {:valid? false :sheets [] :errors ["Failed to parse visual character sheet"]})))
    (catch :default e
      (.error js/console "[ogres:import:parser] visual sheet parse error" (.-message e))
      {:valid? false :sheets [] :errors [(.-message e)]})))

(defn ^:private parse-roll20-stat-block-text [text]
  (try
    (let [text (normalize-text text)]
      (if-not (roll20-stat-block? text)
        {:valid? false :sheets [] :errors ["No Roll20 stat block found in PDF"]}
        (if-let [sheet (parse-roll20-stat-block text)]
          {:valid? true :sheets [sheet] :errors []}
          {:valid? false :sheets [] :errors ["Failed to parse Roll20 stat block"]})))
    (catch :default e
      (.error js/console "[ogres:import:parser] roll20 stat block parse error" (.-message e))
      {:valid? false :sheets [] :errors [(.-message e)]})))

(defn ^:private roll20-sheet? [text]
  (and (re-find #"(?i)\bNAME\b" text)
       (re-find #"(?i)ARMOR\s*\n\s*CLASS" text)
       (re-find #"(?i)STRENGTH\s*\n\s*[+-]?\d+\s*\n\s*MODIFIER" text)))

(defn ^:private parse-roll20 [text]
  (when-let [name (roll20-name text)]
    (let [actions (roll20-actions text)]
      (cond-> {:name name}
        (roll20-class text) (assoc :type (str/lower-case (roll20-class text)))
        (roll20-species text) (assoc :subtype (roll20-species text))
        (roll20-alignment text) (assoc :alignment (roll20-alignment text))
        (roll20-ac text) (assoc :ac (roll20-ac text))
        (roll20-hp text) (assoc :hp (roll20-hp text))
        (roll20-speed text) (assoc :speed (roll20-speed text))
        (roll20-initiative text) (assoc :initiative (roll20-initiative text))
        (roll20-abilities text) (merge (roll20-abilities text))
        (roll20-passive text) (assoc :passive (roll20-passive text))
        (roll20-languages text) (assoc :languages (roll20-languages text))
        (roll20-resistances text) (assoc :resist (roll20-resistances text))
        (seq actions) (assoc :action actions)
        true (assoc :hasToken true :hasFluff true)))))

(defn parse-roll20-text [text]
  (try
    (let [text (normalize-text text)]
      (if-not (roll20-sheet? text)
        {:valid? false :sheets [] :errors ["No Roll20 character sheet found in PDF"]}
        (if-let [sheet (parse-roll20 text)]
          {:valid? true :sheets [sheet] :errors []}
          (do (.warn js/console "[ogres:import:parser] roll20 sheet detected but failed to parse"
                       #js {:name (roll20-name text)})
              {:valid? false :sheets [] :errors ["Failed to parse Roll20 character sheet"]}))))
    (catch :default e
      (.error js/console "[ogres:import:parser] roll20 parse error" (.-message e))
      {:valid? false :sheets [] :errors [(.-message e)]})))

(defn parse-pdf-text [text]
  (parse-pdf-text* (normalize-pdf-text text)))

(defn ^:private parse-pdf-text* [text]
  (let [roll20 (parse-roll20-text text)]
    (cond
      (:valid? roll20)
      (do (.log js/console "[ogres:import:parser] matched roll20")
          roll20)

      :else
      (let [stat-block (parse-roll20-stat-block-text text)]
        (cond
          (:valid? stat-block)
          (do (.log js/console "[ogres:import:parser] matched roll20 stat block")
              stat-block)

          :else
          (let [visual (parse-visual-character-sheet-text text)]
            (cond
              (:valid? visual)
              (do (.log js/console "[ogres:import:parser] matched visual character sheet")
                  visual)

              :else
              (let [markdown (parse-markdown text)]
                (cond
                  (:valid? markdown)
                  (do (.log js/console "[ogres:import:parser] matched markdown stat block")
                      markdown)

                  :else
                  (do (.log js/console "[ogres:import:parser] no pdf format matched"
                             #js {:roll20-error (first (:errors roll20))
                                  :stat-block-error (first (:errors stat-block))
                                  :visual-sheet-error (first (:errors visual))
                                  :markdown-error (first (:errors markdown))})
                      {:valid? false
                       :sheets []
                       :errors [(first (concat (:errors roll20)
                                               (:errors stat-block)
                                               (:errors visual)
                                               (:errors markdown)))]}))))))))))

(defn ^:private json-sheets [parsed]
  (cond
    (vector? parsed) (vec (filter map? parsed))
    (map? parsed) [parsed]
    :else []))

(defn parse-json-text [text]
  (try
    (let [parsed (js->clj (js/JSON.parse text) :keywordize-keys true)
          sheets (json-sheets parsed)
          results (mapv validate-sheet sheets)]
      (cond
        (empty? sheets)
        {:valid? false :sheets [] :errors ["JSON must be an object or array of character sheets"]}

        (every? :valid? results)
        {:valid? true :sheets (mapv :sheet results) :errors []}

        :else
        {:valid? false :sheets [] :errors (mapcat :errors results)}))
    (catch :default e
      {:valid? false :sheets [] :errors [(.-message e)]})))

(defn parse-document [text format]
  (case format
    :markdown (do (.log js/console "[ogres:import:parser] parsing markdown")
                  (parse-markdown text))
    :json (do (.log js/console "[ogres:import:parser] parsing json")
              (parse-json-text text))
    (do (.log js/console "[ogres:import:parser] unsupported format" (name format))
        {:valid? false :sheets [] :errors [(str "Unsupported format: " format)]})))
