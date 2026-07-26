(ns import-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as str]
            [ogres.app.import.parser :as parser]))

(def sample-block
  [">## Abominable Yeti"
   ">*Huge Monstrosity, Chaotic Evil*"
   ">___"
   ">- **Armor Class** 15"
   ">- **Hit Points** 137 (11d12 + 66)"
   ">- **Speed** 40 ft., Climb 40 ft."
   ">- **Initiative** +4 (14)"
   ">___"
   ">|STR|DEX|CON|INT|WIS|CHA|"
   ">|:---:|:---:|:---:|:---:|:---:|:---:|"
   ">|24 (+7)|10 (+0)|22 (+6)|9 (-1)|13 (+1)|9 (-1)|"
   ">___"
   ">- **Skills** Perception +9, Stealth +8"
   ">- **Immunities** Cold"
   ">- **Senses** Darkvision 60 ft., Passive Perception 19"
   ">- **Languages** Yeti"
   ">- **Challenge** 9 (XP 5,000; PB +4)"
   ">- **Proficiency Bonus** +4"
   ">___"
   ">***Fear of Fire.*** If the yeti takes Fire damage..."
   ">"
   ">### Actions"
   ">***Multiattack.*** The yeti can use its Chilling Gaze..."
   ">***Claw.*** *Melee Attack Roll:* +11, reach 5 ft."
   ">"
   ">### Legendary Actions"
   ">***Detect.*** The yeti makes a Wisdom (Perception) check."
   ">***Frightful Presence.*** Each creature within 30 feet must succeed on a DC 18 Wisdom saving throw."])

(deftest test-parse-stat-block
  (let [text (str/join "\n" sample-block)
        sheet (first (:sheets (parser/parse-markdown text)))]
    (is (= "Abominable Yeti" (:name sheet)))
    (is (= ["H"] (:size sheet)))
    (is (= "monstrosity" (:type sheet)))
    (is (= ["C" "E"] (:alignment sheet)))
    (is (= [15] (:ac sheet)))
    (is (= 137 (get-in sheet [:hp :average])))
    (is (= "11d12 + 66" (get-in sheet [:hp :formula])))
    (is (= 40 (:walk (:speed sheet))))
    (is (= 40 (:climb (:speed sheet))))
    (is (= 24 (:str sheet)))
    (is (= 9 (:cha sheet)))
    (is (= "+4" (get-in sheet [:initiative :bonus])))
    (is (= "+9" (get-in sheet [:skill :perception])))
    (is (= ["cold"] (:immune sheet)))
    (is (= "9" (:cr sheet)))
    (is (= "+4" (:proficiency-bonus sheet)))
    (is (= 1 (count (:trait sheet))))
    (is (= 2 (count (:action sheet))))
    (is (= 2 (count (:legendary sheet)))
    (is (= "Detect" (get-in sheet [:legendary 0 :name]))))))

(deftest test-parse-markdown-file
  (let [text (str/join "\n" sample-block)
        {:keys [valid? sheets]} (parser/parse-markdown text)]
    (is valid?)
    (is (= 1 (count sheets)))
    (is (= "Abominable Yeti" (:name (first sheets))))))

(deftest test-validate-sheet
  (let [{:keys [valid?]} (parser/validate-sheet {:name "Test" :str 10})]
    (is valid?))
  (let [{:keys [valid? errors]} (parser/validate-sheet {:str 10})]
    (is (not valid?))
    (is (pos? (count errors)))))

(deftest test-parse-json-sheet
  (let [json (str "{"
                   "\"name\":\"Argamon Flamebound\","
                   "\"str\":17,"
                   "\"hp\":{\"average\":64,\"formula\":\"6d10+30\"},"
                   "\"ac\":[{\"ac\":21,\"from\":[\"Plate\"]}],"
                   "\"trait\":[{\"name\":\"Aura\",\"entries\":[\"Allies gain +3.\"]}]"
                   "}")
        {:keys [valid? sheets errors]} (parser/parse-json-text json)
        sheet (first sheets)]
    (is valid? (str "errors: " errors))
    (is (= "Argamon Flamebound" (:name sheet)))
    (is (= 17 (:str sheet)))
    (is (= 64 (get-in sheet [:hp :average])))
    (is (= "Aura" (get-in sheet [:trait 0 :name])))
    (is (= "Allies gain +3." (first (get-in sheet [:trait 0 :entries]))))))

(def ^:private sample-roll20
  (str/join "\n"
            ["ARGAMON FLAMEBOUND"
             "NAME"
             "Noble Crusader"
             "BACKGROUND"
             "Paladin 6"
             "CLASS"
             "Dragonborn - Red"
             "SPECIES  SUBCLASS"
             "ARMOR"
             "CLASS"
             "21"
             "HIT POINTS"
             "TEMP"
             "51"
             "CURRENT"
             "64"
             "MAX"
             "HIT DICE"
             "SPENT"
             "5D10"
             "MAX"
             "STRENGTH"
             "+3"
             "MODIFIER"
             "17"
             "SCORE"
             "DEXTERITY"
             "+1"
             "MODIFIER"
             "12"
             "SCORE"
             "INITIATIVE"
             "+1"
             "SPEED"
             "30 ft."
             "PASSIVE PERCEPTION"
             "10"
             "WEAPONS & DAMAGE CANTRIPS"
             "NAME  ATK/DC DAMAGE  NOTES"
             "Javelin  +6  1d6+3 piercing"
             "Warhammer +1  +7  1d8+4 bludgeoning"
             "ATTACKS"
             "NAME  ATK/DC DAMAGE  RANGE  NOTES"
             "Dragon Fear"
             "WIS 14"
             "—"
             "30 ft"
             "Breath Weapon"
             "DEX 14"
             "2d10 fire"
             "30 feet"
             "ACTIONS"
             "NOTES"
             "Divine Smite"
             "When you hit a creature with a melee weapon attack"
             "ALIGNMENT Lawful Evil"
             "LANGUAGES"
             "Common, Draconic, Infernal"
             "RESISTANCES Fire"]))

(deftest test-parse-roll20-sheet
  (let [{:keys [valid? sheets]} (parser/parse-roll20-text sample-roll20)
        sheet (first sheets)]
    (is valid?)
    (is (= "ARGAMON FLAMEBOUND" (:name sheet)))
    (is (= [21] (:ac sheet)))
    (is (= 64 (get-in sheet [:hp :average])))
    (is (= "5d10" (get-in sheet [:hp :formula])))
    (is (= 17 (:str sheet)))
    (is (= 12 (:dex sheet)))
    (is (= "+1" (get-in sheet [:initiative :bonus])))
    (is (= 30 (:walk (:speed sheet))))
    (is (<= 4 (count (:action sheet))))))

(deftest test-parse-roll20-pdf-text
  (let [pdf-text (str/join "\n\n" (str/split-lines sample-roll20))
        {:keys [valid? sheets]} (parser/parse-pdf-text pdf-text)
        sheet (first sheets)]
    (is valid?)
    (is (= "ARGAMON FLAMEBOUND" (:name sheet)))))

(def ^:private sample-roll20-stat-block
  (str/join "\n"
            ["James May"
             "Medium humanoid (human), lawful good"
             "Hit Points"
             "44"
             "("
             "17d10+85"
             ")"
             "Speed"
             "30 ft"
             "Skills"
             "Insight"
             "+"
             "4"
             ","
             "Persuasion"
             "+"
             "8"
             "Damage Resistances"
             "Radiant, Poison"
             "Senses"
             "Darkvision 60 ft., passive Perception 15"
             "Languages"
             "Common, Celestial"
             "Challenge"
             "10"
             "("
             "5900"
             "XP)"
             "Armor Class"
             "18"
             "("
             "Plate armor"
             ")"
             "STR"
             "16"
             "(+"
             "3"
             ")"
             "DEX"
             "12"
             "(+"
             "1"
             ")"
             "CON"
             "20"
             "(+"
             "5"
             ")"
             "INT"
             "14"
             "(+"
             "2"
             ")"
             "WIS"
             "10"
             "(+"
             "0"
             ")"
             "CHA"
             "18"
             "(+"
             "4"
             ")"
             "Guardian's Protection"
             "As a reaction, grant temporary hit points."
             "Actions"
             "Divine Strike"
             "."
             "Melee Weapon Attack: +10 to hit, reach 5 ft., one target. Hit:"
             "13 (2d6 + 6) radiant damage."]))

(deftest test-parse-roll20-stat-block
  (let [{:keys [valid? sheets]} (parser/parse-pdf-text sample-roll20-stat-block)
        sheet (first sheets)]
    (is valid?)
    (is (= "James May" (:name sheet)))
    (is (= ["M"] (:size sheet)))
    (is (= "humanoid" (:type sheet)))
    (is (= "human" (:subtype sheet)))
    (is (= [18] (:ac sheet)))
    (is (= 44 (get-in sheet [:hp :average])))
    (is (= "17d10+85" (get-in sheet [:hp :formula])))
    (is (= 16 (:str sheet)))
    (is (= 18 (:cha sheet)))
    (is (= 15 (:passive sheet)))
    (is (= "10" (:cr sheet)))
    (is (= 1 (count (:trait sheet))))
    (is (= "Guardian's Protection" (get-in sheet [:trait 0 :name])))
    (is (= "As a reaction, grant temporary hit points."
           (first (get-in sheet [:trait 0 :entries]))))
    (is (pos? (count (:action sheet))))
    (is (= "Divine Strike" (:name (first (:action sheet)))))))

(def ^:private sample-visual-sheet
  (str/join "\n"
            ["\"Flambel the Magnificent\" - Andain Belladum"
             "0" "4" "0" "0"
             "Charlatan" "Bard" "13" "24" "4d8" "Human" "College of Lore"
             "+2" "30ft" "5'8\"" "12"
             "Bardic Inspiration 1d6"
             "Expertise: Deception and Performance"
             "Jack of All Trades"
             "Cutting Words"
             "Charisma"
             "Vicious Mockery" "Minor Illusion" "Healing Word"
             "Common, Elvish, Goblin"]))

(deftest test-parse-visual-character-sheet
  (let [{:keys [valid? sheets errors]} (parser/parse-pdf-text sample-visual-sheet)
        sheet (first sheets)]
    (is valid? (str "errors: " errors))
    (is (= "Flambel the Magnificent" (:name sheet)))
    (is (= [12] (:ac sheet)))
    (is (= 24 (get-in sheet [:hp :average])))
    (is (= "4d8" (get-in sheet [:hp :formula])))
    (is (= "human" (:subtype sheet)))
    (is (= "humanoid" (:type sheet)))
    (is (= 30 (get-in sheet [:speed :walk])))
    (is (= "+2" (:proficiency-bonus sheet)))
    (is (= "4" (:cr sheet)))
    (is (pos? (count (:trait sheet))))
    (is (= "Bardic Inspiration 1d6" (:name (first (:trait sheet)))))
    (is (= ["Common" "Elvish" "Goblin"] (:languages sheet)))
    (is (some #(= "Spells" (:name %)) (:trait sheet)))))

(deftest test-parse-json-spellcasting
  (let [json (str "{"
                  "\"name\":\"Garuun\","
                  "\"ac\":[13],"
                  "\"hp\":{\"average\":53,\"formula\":\"6d8 + 18\"},"
                  "\"str\":10,"
                  "\"spellcasting\":[{\"name\":\"Spellcasting\","
                  "\"headerEntries\":[\"Wisdom is his spellcasting ability.\"],"
                  "\"ability\":\"wis\","
                  "\"spells\":{\"0\":{\"spells\":[\"Guidance\",\"Druidcraft\"]},"
                  "\"1\":{\"slots\":4,\"spells\":[\"Cure Wounds\"]}}}]"
                  "}")
        sheet (first (:sheets (parser/parse-json-text json)))
        block (first (:spellcasting sheet))
        cantrips (get-in block [:spells :0 :spells]
                    (get-in block [:spells "0" :spells]))
        first-level (get-in block [:spells :1 :spells]
                      (get-in block [:spells "1" :spells]))]
    (is (= "Garuun" (:name sheet)))
    (is (= "Spellcasting" (:name block)))
    (is (= :wis (:ability block)))
    (is (= ["Guidance" "Druidcraft"] cantrips))
    (is (= 4 (or (get-in block [:spells :1 :slots])
                 (get-in block [:spells "1" :slots]))))
    (is (= ["Cure Wounds"] first-level))))
