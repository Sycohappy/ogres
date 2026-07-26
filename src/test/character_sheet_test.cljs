(ns character-sheet-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as str]
            [datascript.core :as ds :refer [transact! entity]]
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
    (is (re-find #"— \d+ \(d20 \+6\)$" body)))

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
    (is (= [{:count 2 :sides 6 :modifier 7}
            {:count 2 :sides 6 :modifier 0}]
           (initiative/parse-damage-expressions body)))
    (is (initiative/attack-message? body))
    (is (not (initiative/attack-message? "hello")))))

(deftest test-damage-chat-body-format
  (let [body (str "Claw. *Melee Attack Roll:* +11. *Hit:* 14 (2d6 + 7) Slashing damage. — 17 (d20 +11)")
        result (initiative/damage-chat-body body)]
    (is (string? result))
    (is (str/starts-with? result "Claw damage — "))
    (is (re-find #"2d6\+7:" result))))

(deftest test-saving-throw-damage
  (let [body "Chilling Gaze. *Constitution Saving Throw:* DC 18. *Failure:* 21 (6d6) Cold damage."]
    (is (initiative/saving-throw-message? body))
    (is (initiative/damage-message? body))
    (is (= [{:count 6 :sides 6 :modifier 0}]
           (initiative/parse-damage-expressions body)))
    (let [result (initiative/damage-chat-body body)]
      (is (string? result))
      (is (str/starts-with? result "Chilling Gaze damage — "))
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
      (is (= {:name "New Character"} (:character-sheet/data sheet))))))

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
