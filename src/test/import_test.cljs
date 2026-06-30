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
