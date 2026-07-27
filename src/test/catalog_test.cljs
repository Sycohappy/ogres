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
            :entries ["Bless up to three creatures."]}]})

(deftest test-five-etools-mappers
  (let [{:keys [feature resource]} (five/class-feature->v2 (first (:classFeature sample-5etools)))]
    (is (= "Lay on Hands" (:name feature)))
    (is (str/starts-with? (:source feature) "5etools:classFeature:"))
    (is (= 25 (:max resource))))
  (let [{:keys [item attack]} (five/item->v2 (first (:item sample-5etools)))]
    (is (= "Warhammer +1" (:name item)))
    (is (= 2 (count (:damage attack)))))
  (is (= 1 (:level (five/spell->v2 (first (:spell sample-5etools)))))))

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
