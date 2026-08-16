(ns catalog-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as str]
            [ogres.app.catalog.core :as catalog]
            [ogres.app.catalog.five-etools :as five]
            [ogres.app.character-sheet :as sheet]))

(def sample-5etools
  {:classFeature [{:name "Lay on Hands"
                   :source "PHB"
                   :entries ["You have a pool of healing power."]
                   :uses 25
                   :recharge "long-rest"}]
   :item [{:name "Warhammer +1"
           :source "DMG"
           :dmg1 "1d8"
           :dmg2 "1d10"
           :dmgType "bludgeoning"
           :bonusWeapon "+1"
           :entries ["A magical warhammer."]}]
   :spell [{:name "Bless"
            :source "PHB"
            :level 1
            :school "E"
            :time [{:number 1 :unit "action"}]
            :range {:type "point" :distance {:type "feet" :amount 30}}
            :components {:v true :s true :m "a sprinkling of holy water"}
            :duration [{:type "timed" :duration {:type "minute" :amount 1}
                        :concentration true}]
            :entries ["Bless up to three creatures."]
            :entriesHigherLevel ["When you cast this spell using a spell slot of 2nd level or higher, you can target one additional creature for each slot level above 1st."]}]})

(def sample-spells-xge
  {:spell [{:name "Catapult"
            :source "XGE"
            :level 1
            :school "T"
            :time [{:number 1 :unit "action"}]
            :range {:type "point" :distance {:type "feet" :amount 60}}
            :components {:s true}
            :duration [{:type "instant"}]
            :entries ["Choose one object weighing 1 to 5 pounds within range."]}]})

(deftest test-five-etools-mappers
  (let [{:keys [feature resource]} (five/class-feature->v2 (first (:classFeature sample-5etools)))]
    (is (= "Lay on Hands" (:name feature)))
    (is (str/starts-with? (:source feature) "5etools:classFeature:"))
    (is (= 25 (:max resource))))
  (let [{:keys [item attack]} (five/item->v2 (first (:item sample-5etools)))]
    (is (= "Warhammer +1" (:name item)))
    (is (= 2 (count (:damage attack)))))
  (let [spell (five/spell->v2 (first (:spell sample-5etools)))]
    (is (= 1 (:level spell)))
    (is (= "Enchantment" (:school spell)))
    (is (= "1 action" (:time spell)))
    (is (= "30 feet" (:range spell)))
    (is (str/includes? (:components spell) "V"))
    (is (str/includes? (:duration spell) "Concentration"))
    (is (true? (:concentration spell)))
    (is (= 2 (count (:entries spell))))
    (is (str/includes? (second (:entries spell)) "At Higher Levels")))
  (let [smite (five/spell->v2
               {:name "Wrathful Smite"
                :source "PHB"
                :level 1
                :school "V"
                :savingThrow ["wisdom"]
                :spellAttack []
                :damageInflict ["psychic"]
                :entries ["deals an extra {@damage 1d6} psychic damage."]})]
    (is (= ["wisdom"] (:savingThrow smite)))
    (is (= 1 (count (:damage smite))))
    (is (= 6 (get-in smite [:damage 0 :sides])))
    (is (= "psychic" (get-in smite [:damage 0 :type])))
    (is (str/includes? (first (:entries smite)) "1d6"))
    (is (not (str/includes? (first (:entries smite)) "@damage"))))
  (let [weapon (five/spell->v2
                {:name "Spiritual Weapon"
                 :source "PHB"
                 :level 2
                 :spellAttack ["M"]
                 :damageInflict ["force"]
                 :entries ["make a melee spell attack. Hit: {@damage 1d8} force damage."]})]
    (is (true? (:spellAttack weapon)))
    (is (= "force" (get-in weapon [:damage 0 :type])))))

(deftest test-catalog-apply
  (catalog/clear!)
  (let [result (catalog/load-data! sample-5etools "test.json")
        entry (first (catalog/search "Lay on Hands"))
        sheet (sheet/ensure-runtime
               {:version 2 :identity {:name "Test"} :features [] :resources []
                :runtime {:hp {:current 10 :temp 0} :resourceSpent {} :slotsExpended {} :effects []}})
        next (catalog/apply-entry sheet entry)]
    (is (= 3 (:count result)))
    (is (= 1 (count (:features next))))
    (is (= 1 (count (:resources next))))
    (catalog/clear!)))

(deftest test-spell-apply-index
  (catalog/clear!)
  (catalog/load-data! sample-5etools "phb.json")
  (let [entry (first (catalog/search "Bless"))
        sheet (sheet/ensure-runtime
               {:version 2 :identity {:name "Caster"} :spellcasting []
                :runtime {:hp {:current 10 :temp 0} :resourceSpent {} :slotsExpended {} :effects []}})
        next (catalog/apply-entry sheet entry)
        again (catalog/apply-entry next entry)
        indexed (get-in next [:spellIndex "bless"])]
    (is (= "Bless" (:name indexed)))
    (is (= "Enchantment" (:school indexed)))
    (is (= ["Bless"] (get-in next [:spellcasting 0 :prepared "1"])))
    (is (= ["Bless"] (get-in again [:spellcasting 0 :prepared "1"]))
        "re-applying the same spell does not duplicate prepared list")
    (is (= indexed (sheet/find-spell next "Bless")))
    (catalog/clear!)))

(deftest test-catalog-merge-load
  (catalog/clear!)
  (let [first-load (catalog/load-data! sample-5etools "phb.json")
        second-load (catalog/load-data! sample-spells-xge "xge.json" true)
        bless (first (catalog/search "Bless"))
        catapult (first (catalog/search "Catapult"))]
    (is (false? (:merged first-load)))
    (is (true? (:merged second-load)))
    (is (= 4 (:count second-load)))
    (is (= :spell (:kind bless)))
    (is (= :spell (:kind catapult)))
    (is (= ["phb.json" "xge.json"] (catalog/sources)))
    (catalog/clear!)))
