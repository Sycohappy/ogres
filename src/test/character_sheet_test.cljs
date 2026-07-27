(ns character-sheet-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as str]
            [datascript.core :as ds :refer [transact! entity]]
            [ogres.app.character-sheet :as sheet]
            [ogres.app.events :refer [event-tx-fn]]
            [ogres.app.initiative :as initiative]
            [ogres.app.provider.state :refer [initial-data]]
            [ogres.app.vec :refer [Vec2]]))

(defn dispatch [conn event & args]
  (transact! conn [[:db.fn/call (fn [db] (apply event-tx-fn db event args))]]))

(def sample-sheet
  {:name "Abominable Yeti"
   :cr "9"
   :ac [15]
   :hp {:average 137 :formula "11d12 + 66"}
   :str 24})

(def sample-v2-attack
  {:id "warhammer-1"
   :name "Warhammer +1"
   :kind :attack
   :bonus 7
   :description "Melee Attack Roll: +7. Hit: 8 (1d8 + 4) bludgeoning."
   :damage [{:id "1h" :count 1 :sides 8 :modifier 4 :type "bludgeoning"}
            {:id "2h" :count 1 :sides 10 :modifier 4 :type "bludgeoning"}]})

(def sample-v2-sheet
  {:version 2
   :name "Argamon Flamebound"
   :identity {:name "Argamon Flamebound" :class "Paladin" :level 6}
   :vitals {:ac {:value 21 :from ["Plate"]}
            :hp {:max 64 :formula "6d10+30"}
            :initiative {:bonus 1}
            :proficiencyBonus 3}
   :abilities {:str {:score 17 :modifier 3 :save 6 :proficient false}
               :dex {:score 12 :modifier 1 :save 1 :proficient false}
               :cha {:score 16 :modifier 3 :save 9 :proficient true}}
   :attacks [sample-v2-attack]
   :resources [{:id "lay-on-hands" :name "Lay on Hands" :kind "pool" :max 25 :recharge "long-rest"}]
   :spellcasting [{:name "Paladin"
                   :ability "cha"
                   :dc 14
                   :slots {"1" 4 "2" 2}
                   :prepared {"1" ["command"] "2" ["aid"]}}]
   :runtime {:hp {:current 64 :temp 0} :resourceSpent {} :slotsExpended {} :effects []}})

(deftest test-attack-modifier
  (is (= 11 (initiative/attack-modifier "*Melee Attack Roll:* +11, reach 5 ft.")))
  (is (= 7 (initiative/attack-modifier "Ranged Attack Roll: +7, range 60/240 ft.")))
  (is (= 6 (initiative/attack-modifier
            "Melee Spell Attack: +6 to hit, reach 5 ft., one target.")))
  (is (= 6 (initiative/attack-modifier
            "Ranged Spell Attack: +6 to hit, range 60 ft., one target.")))
  (is (= 10 (initiative/attack-modifier
             "Melee Weapon Attack: +10 to hit, reach 5 ft., one target.")))
  (is (= 6 (initiative/attack-modifier
            "*Melee Weapon Attack:* +6 to hit, reach 5 ft., one target. *Hit:* 1d8 bludgeoning damage.")))
  (is (nil? (initiative/attack-modifier "*Constitution Saving Throw:* DC 18")))
  (is (nil? (initiative/attack-modifier "The yeti makes two attacks"))))

(deftest test-action-chat-body-spell-attack
  (let [body (initiative/action-chat-body
              "Shillelagh"
              "Melee Spell Attack: +6 to hit, reach 5 ft., one target.")]
    (is (str/starts-with? body "Shillelagh. Melee Spell Attack: +6"))
    (is (re-find #"— \d+ \(d20 \+6\)$" body))))

(deftest test-action-chat-body-markdown-weapon-attack
  (let [body (initiative/action-chat-body
              "Staff of Necrotic"
              "*Melee Weapon Attack:* +6 to hit, reach 5 ft., one target. *Hit:* 1d8 bludgeoning damage.")]
    (is (str/starts-with? body "Staff of Necrotic."))
    (is (re-find #"— \d+ \(d20 \+6\)$" body))))

(deftest test-action-chat-body-attack-roll
  (let [body (initiative/action-chat-body
              "Claw"
              "*Melee Attack Roll:* +11, reach 5 ft. *Hit:* 14 damage.")
        roll-start (str/index-of body " — ")]
    (is (some? roll-start))
    (is (str/starts-with? body "Claw. *Melee Attack Roll:* +11"))
    (is (re-find #"— \d+ \(d20 \+11\)$" body))))

(deftest test-action-chat-body-non-attack
  (is (= "Multiattack. The yeti makes two attacks."
         (initiative/action-chat-body "Multiattack" "The yeti makes two attacks."))))

(deftest test-parse-damage-expressions
  (let [body (str "Claw. *Melee Attack Roll:* +11, reach 5 ft. *Hit:* 14 (2d6 + 7) Slashing "
                  "damage plus 7 (2d6) Cold damage. — 17 (d20 +11)")]
    (is (= [{:count 2 :sides 6 :modifier 7 :type "Slashing"}
            {:count 2 :sides 6 :modifier 0 :type "Cold"}]
           (initiative/parse-damage-expressions body)))
    (is (= "2d6+7 slashing" (initiative/damage-button-label (first (initiative/parse-damage-expressions body)))))
    (is (= "2d6 cold" (initiative/damage-button-label (second (initiative/parse-damage-expressions body)))))
    (is (initiative/attack-message? body))
    (is (not (initiative/attack-message? "hello")))))

(deftest test-parse-versatile-weapon-damage
  (let [body (str "Warhammer +1. *Melee Attack Roll:* +7, reach 5 ft., one target. "
                  "*Hit:* 8 (1d8 + 4) bludgeoning damage, or 9 (1d10 + 4) bludgeoning "
                  "damage if used with two hands. — 15 (d20 +7)")
        exprs (initiative/parse-damage-expressions body)]
    (is (= [{:count 1 :sides 8 :modifier 4 :type "bludgeoning"}
            {:count 1 :sides 10 :modifier 4 :type "bludgeoning"}]
           exprs))
    (is (= "1d8+4 bludgeoning" (initiative/damage-button-label (first exprs))))
    (is (= "1d10+4 bludgeoning" (initiative/damage-button-label (second exprs))))))

(deftest test-damage-chat-body-format
  (let [body (str "Claw. *Melee Attack Roll:* +11. *Hit:* 14 (2d6 + 7) Slashing damage. — 17 (d20 +11)")
        result (initiative/damage-chat-body body 0)]
    (is (string? result))
    (is (str/starts-with? result "Claw damage slashing — "))
    (is (re-find #"2d6\+7:" result))))

(deftest test-damage-chat-body-selects-index
  (let [body (str "Claw. *Melee Attack Roll:* +11. *Hit:* 14 (2d6 + 7) Slashing "
                  "damage plus 7 (2d6) Cold damage. — 17 (d20 +11)")
        slash (initiative/damage-chat-body body 0)
        cold (initiative/damage-chat-body body 1)]
    (is (re-find #"2d6\+7:" slash))
    (is (str/includes? slash "slashing"))
    (is (not (str/includes? slash "cold")))
    (is (re-find #"2d6:" cold))
    (is (str/includes? cold "cold"))
    (is (not (re-find #"2d6\+7:" cold)))
    (is (nil? (initiative/damage-chat-body body 2)))))

(deftest test-saving-throw-damage
  (let [body "Chilling Gaze. *Constitution Saving Throw:* DC 18. *Failure:* 21 (6d6) Cold damage."]
    (is (initiative/saving-throw-message? body))
    (is (initiative/damage-message? body))
    (is (= [{:count 6 :sides 6 :modifier 0 :type "Cold"}]
           (initiative/parse-damage-expressions body)))
    (let [result (initiative/damage-chat-body body 0)]
      (is (string? result))
      (is (str/starts-with? result "Chilling Gaze damage cold — "))
      (is (re-find #"6d6:" result)))))

(deftest test-import-character-sheet
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :character-sheets/import [sample-sheet] "bestiary.md")
    (let [root (entity @conn [:db/ident :root])
          sheets (:root/character-sheets root)]
      (is (= 1 (count sheets)))
      (is (= "Abominable Yeti" (:character-sheet/name (first sheets))))
      (is (= sample-sheet (:character-sheet/data (first sheets)))))))

(deftest test-create-blank-character-sheet
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :character-sheets/create-blank)
    (let [sheet (first (:root/character-sheets
                        (entity @conn [:db/ident :root])))]
      (is (string? (:character-sheet/id sheet)))
      (is (= "New Character" (:character-sheet/name sheet)))
      (is (sheet/v2? (:character-sheet/data sheet)))
      (is (= "New Character" (get-in sheet [:character-sheet/data :identity :name]))))))

(deftest test-spend-resource-and-rest
  (let [conn (ds/conn-from-db (initial-data true))
        id "sheet-v2"
        data sample-v2-sheet]
    (transact! conn
               [{:db/ident :root
                 :root/character-sheets
                 [{:character-sheet/id id
                   :character-sheet/name "Argamon"
                   :character-sheet/data data}]}])
    (dispatch conn :character-sheets/spend-resource id "lay-on-hands" 5)
    (let [sheet (:character-sheet/data (entity @conn [:character-sheet/id id]))]
      (is (= 5 (sheet/resource-spent sheet "lay-on-hands")))
      (is (= 20 (sheet/resource-remaining sheet "lay-on-hands"))))
    (dispatch conn :character-sheets/expend-slot id "1" 1)
    (dispatch conn :character-sheets/expend-slot id "1" 1)
    (let [sheet (:character-sheet/data (entity @conn [:character-sheet/id id]))]
      (is (= 2 (sheet/slots-expended sheet "1")))
      (is (= 2 (sheet/slots-remaining sheet "1")))
      (is (= 2 (sheet/slots-remaining sheet "2"))))
    (dispatch conn :character-sheets/rest id :long-rest)
    (let [sheet (:character-sheet/data (entity @conn [:character-sheet/id id]))]
      (is (= 0 (sheet/resource-spent sheet "lay-on-hands")))
      (is (= 0 (sheet/slots-expended sheet "1")))
      (is (= 64 (get-in sheet [:runtime :hp :current]))))))

(deftest test-change-hp-temp-first-and-token-health
  (let [conn (ds/conn-from-db (initial-data true))
        id "sheet-hp"
        data (-> sample-v2-sheet
                 (assoc-in [:runtime :hp] {:current 50 :temp 5}))
        scene-id (:db/id (:camera/scene
                          (:user/camera (entity @conn [:db/ident :user]))))]
    (transact! conn
               [{:db/ident :root
                 :root/character-sheets
                 [{:character-sheet/id id
                   :character-sheet/name "Argamon"
                   :character-sheet/data data}]
                 :root/token-images
                 [{:image/hash "hp-tok"
                   :token-image/character-sheet data}]}
                {:db/id -10
                 :object/type :token/token
                 :token/character-sheet data
                 :token/image [:image/hash "hp-tok"]
                 :initiative/health 50}
                [:db/add scene-id :scene/tokens -10]])
    (dispatch conn :character-sheets/change-hp id 8)
    (let [sheet (:character-sheet/data (entity @conn [:character-sheet/id id]))
          token (first (:scene/tokens (entity @conn scene-id)))
          snap (sheet/runtime-snapshot sheet)]
      (is (= 47 (get-in sheet [:runtime :hp :current])))
      (is (= 0 (get-in sheet [:runtime :hp :temp])))
      (is (= 47 (:initiative/health token)))
      (is (= {:current 47 :temp 0 :max 64} (:hp snap))))
    (dispatch conn :character-sheets/change-hp id -10)
    (let [sheet (:character-sheet/data (entity @conn [:character-sheet/id id]))
          token (first (:scene/tokens (entity @conn scene-id)))]
      (is (= 57 (get-in sheet [:runtime :hp :current])))
      (is (= 0 (get-in sheet [:runtime :hp :temp])))
      (is (= 57 (:initiative/health token))))
    (dispatch conn :character-sheets/rest id :long-rest)
    (let [sheet (:character-sheet/data (entity @conn [:character-sheet/id id]))
          token (first (:scene/tokens (entity @conn scene-id)))]
      (is (= 64 (get-in sheet [:runtime :hp :current])))
      (is (= 64 (:initiative/health token))))))

(deftest test-token-link-sheet-marks-pc-and-syncs-hp
  (let [conn (ds/conn-from-db (initial-data true))
        id "sheet-link"
        data sample-v2-sheet
        scene-id (:db/id (:camera/scene
                          (:user/camera (entity @conn [:db/ident :user]))))]
    (transact! conn
               [{:db/ident :root
                 :root/character-sheets
                 [{:character-sheet/id id
                   :character-sheet/name "Argamon"
                   :character-sheet/data data}]
                 :root/token-images
                 [{:image/hash "pc-tok" :image/name "pc.png"}]}
                {:db/id -10
                 :object/type :token/token
                 :token/image [:image/hash "pc-tok"]
                 :token/label ""}
                [:db/add scene-id :scene/tokens -10]])
    (let [token-id (:db/id (first (:scene/tokens (entity @conn scene-id))))]
      (dispatch conn :token/change-character-sheet [token-id] data)
      (let [token (entity @conn token-id)]
        (is (= data (:token/character-sheet token)))
        (is (= id (:token/character-sheet-id token)))
        (is (contains? (:token/flags token) :player))
        (is (= 64 (:initiative/health token)))
        (is (= "Argamon Flamebound" (:token/label token))))
      (dispatch conn :initiative/change-health token-id (fn [_ v] v) "40")
      (let [sheet (:character-sheet/data (entity @conn [:character-sheet/id id]))
            token (entity @conn token-id)]
        (is (= 40 (:initiative/health token)))
        (is (= 40 (get-in sheet [:runtime :hp :current])))
        (is (= id (:token/character-sheet-id token))))
      ;; After runtime drift, id-based sync still updates the token HP.
      (dispatch conn :character-sheets/change-hp id 5)
      (let [token (entity @conn token-id)
            sheet (:character-sheet/data (entity @conn [:character-sheet/id id]))]
        (is (= 35 (get-in sheet [:runtime :hp :current])))
        (is (= 35 (:initiative/health token)))
        (is (= id (:token/character-sheet-id token))))
      (dispatch conn :token-images/change-character-sheet "pc-tok" nil)
      (let [token (entity @conn token-id)
            image (entity @conn [:image/hash "pc-tok"])]
        (is (nil? (:token-image/character-sheet image)))
        (is (nil? (:token-image/character-sheet-id image)))
        (is (nil? (:token/character-sheet token)))
        (is (nil? (:token/character-sheet-id token)))))))

(deftest test-add-effect-ticks-on-round
  (let [conn (ds/conn-from-db (initial-data true))
        id "sheet-fx"
        data (assoc-in sample-v2-sheet [:runtime :effects]
                       [{:id "e1" :name "Bless" :rounds-remaining 2 :concentration true}])]
    (transact! conn
               [{:db/ident :root
                 :root/character-sheets
                 [{:character-sheet/id id
                   :character-sheet/name "Argamon"
                   :character-sheet/data data}]}])
    (let [user (entity @conn [:db/ident :user])
          scene (:camera/scene (:user/camera user))]
      (transact! conn [{:db/id (:db/id scene) :initiative/rounds 1}])
      (dispatch conn :initiative/next)
      (let [sheet (:character-sheet/data (entity @conn [:character-sheet/id id]))
            fx (get-in sheet [:runtime :effects])]
        (is (= 1 (count fx)))
        (is (= 1 (:rounds-remaining (first fx))))))))

(deftest test-update-character-sheet-refreshes-linked-copies
  (let [conn (ds/conn-from-db (initial-data true))
        id "sheet-1"
        updated (assoc sample-sheet :name "Frost Yeti" :ac [16])
        scene-id (:db/id (:camera/scene
                          (:user/camera (entity @conn [:db/ident :user]))))]
    (transact! conn
               [{:db/ident :root
                 :root/character-sheets
                 [{:character-sheet/id id
                   :character-sheet/name (:name sample-sheet)
                   :character-sheet/data sample-sheet}]
                 :root/token-images
                 [{:image/hash "linked"
                   :token-image/character-sheet sample-sheet}]}
                {:db/id -10
                 :object/type :token/token
                 :token/character-sheet sample-sheet}
                [:db/add scene-id :scene/tokens -10]])
    (dispatch conn :character-sheets/update id updated)
    (let [entry (entity @conn [:character-sheet/id id])
          image (entity @conn [:image/hash "linked"])
          token (first (:scene/tokens (entity @conn scene-id)))]
      (is (= "Frost Yeti" (:character-sheet/name entry)))
      (is (= updated (:character-sheet/data entry)))
      (is (= updated (:token-image/character-sheet image)))
      (is (= updated (:token/character-sheet token))))))

(deftest test-token-image-character-sheet-copy
  (let [conn (ds/conn-from-db (initial-data true))]
    (transact! conn
               [{:db/ident :root
                 :root/token-images
                 [{:image/hash "abc"
                   :image/name "yeti.png"
                   :token-image/character-sheet sample-sheet}]}])
    (dispatch conn :token/create (Vec2. 100 100) "abc")
    (let [tokens (:scene/tokens
                  (:camera/scene (:user/camera (entity @conn [:db/ident :user]))))]
      (is (= 1 (count tokens)))
      (is (= sample-sheet (:token/character-sheet (first tokens)))))))

(deftest test-token-image-size-and-light-defaults
  (let [conn (ds/conn-from-db (initial-data true))]
    (transact! conn
               [{:db/ident :root
                 :root/token-images [{:image/hash "defaults"
                                      :image/name "defaults.png"}]}])
    (dispatch conn :token-images/change-default-size "defaults" 10)
    (dispatch conn :token-images/change-default-light "defaults" 30)
    (dispatch conn :token/create (Vec2. 100 100) "defaults")
    (let [image (entity @conn [:image/hash "defaults"])
          token (first
                 (:scene/tokens
                  (:camera/scene
                   (:user/camera (entity @conn [:db/ident :user])))))]
      (is (= 10 (:token-image/default-size image)))
      (is (= 30 (:token-image/default-light image)))
      (is (= 10 (:token/size token)))
      (is (= 30 (:token/light token))))
    (dispatch conn :token-images/change-default-size "defaults" nil)
    (dispatch conn :token-images/change-default-light "defaults" nil)
    (let [image (entity @conn [:image/hash "defaults"])]
      (is (nil? (:token-image/default-size image)))
      (is (nil? (:token-image/default-light image))))))

(deftest test-clipboard-copies-character-sheet
  (let [conn (ds/conn-from-db (initial-data true))
        sheet {:name "Test" :cr "1"}]
    (transact! conn
               [{:db/ident :root
                 :root/token-images [{:image/hash "tok" :image/name "t.png"}]}
                {:db/id -10
                 :object/type :token/token
                 :token/label "Test"
                 :token/character-sheet sheet
                 :token/image [:image/hash "tok"]}
                [:db/add (:db/id (:camera/scene (:user/camera (entity @conn [:db/ident :user]))))
                 :scene/tokens -10]
                [:db/add (:db/id (:user/camera (entity @conn [:db/ident :user])))
                 :camera/selected -10]])
    (dispatch conn :clipboard/copy)
    (dispatch conn :clipboard/paste)
    (let [tokens (:scene/tokens
                  (:camera/scene (:user/camera (entity @conn [:db/ident :user]))))]
      (is (= 2 (count tokens)))
      (is (every? #(= sheet (:token/character-sheet %)) tokens)))))

(deftest test-v2-adapter-basics
  (is (sheet/v2? sample-v2-sheet))
  (is (not (sheet/v2? sample-sheet)))
  (is (= "Argamon Flamebound" (sheet/sheet-name sample-v2-sheet)))
  (is (= 21 (sheet/ac-value sample-v2-sheet)))
  (is (= 64 (sheet/hp-max sample-v2-sheet)))
  (is (= 17 (sheet/ability-score sample-v2-sheet :str)))
  (is (= 1 (sheet/initiative-bonus sample-v2-sheet)))
  (is (= 1 (count (sheet/attacks sample-v2-sheet))))
  (is (= 2 (count (sheet/damage-options sample-v2-attack)))))

(deftest test-structured-damage-dual-read
  (let [body (sheet/attack-chat-body sample-v2-attack)
        structured (sheet/damage-options sample-v2-attack)
        exprs (initiative/damage-expressions body structured)]
    (is (str/includes? body "Warhammer +1"))
    (is (re-find #"— \d+ \(d20 \+7\)$" body))
    (is (= 2 (count exprs)))
    (is (= 8 (:sides (first exprs))))
    (is (= 10 (:sides (second exprs))))
    (let [one-hand (initiative/damage-chat-body body 0 structured)
          two-hand (initiative/damage-chat-body body 1 structured)]
      (is (re-find #"1d8\+4:" one-hand))
      (is (re-find #"1d10\+4:" two-hand)))))

(deftest test-convert-v1-to-v2
  (let [v2 (sheet/convert-v1->v2 sample-sheet)]
    (is (sheet/v2? v2))
    (is (= "Abominable Yeti" (sheet/sheet-name v2)))
    (is (= 15 (sheet/ac-value v2)))
    (is (= 137 (sheet/hp-max v2)))
    (is (= 24 (sheet/ability-score v2 :str)))))

(deftest test-v2-initiative-modifier
  (is (= 1 (initiative/modifier-from-sheet sample-v2-sheet)))
  (is (= 1 (initiative/modifier-from-sheet {:dex 12}))))
