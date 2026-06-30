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
  (is (nil? (initiative/attack-modifier "*Constitution Saving Throw:* DC 18")))
  (is (nil? (initiative/attack-modifier "The yeti makes two attacks"))))

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
