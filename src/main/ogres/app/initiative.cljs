(ns ogres.app.initiative
  (:require [clojure.string :as str]
            [datascript.core :as ds]))

(defn parse-modifier
  "Parses a modifier string such as \"+4\" into an integer."
  [value]
  (when (some? value)
    (let [parsed (js/parseInt (str value) 10)]
      (when-not (js/isNaN parsed) parsed))))

(defn ^:private format-signed-mod [n]
  (str (if (neg? n) "" "+") n))

(defn attack-modifier
  "Parses the to-hit modifier from an action description, if present."
  [description]
  (when (and (string? description)
             (re-find #"(?i)attack\s+roll" description))
    (when-let [m (re-find #"(?i)attack\s+roll[^+\d-]*([+-]\d+)" description)]
      (parse-modifier (nth m 1)))))

(defn attack-roll-result
  "Rolls d20 plus the attack modifier parsed from the description."
  [description]
  (when-let [modifier (attack-modifier description)]
    (let [die (inc (rand-int 20))]
      {:die die :modifier modifier :total (+ die modifier)})))

(defn format-attack-roll
  [{:keys [die modifier total]}]
  (str total " (d20 " (format-signed-mod modifier) ")"))

(defn attack-message?
  "True when the chat body looks like a sheet attack action."
  [text]
  (and (string? text) (re-find #"(?i)attack\s+roll" text)))

(defn saving-throw-message?
  "True when the chat body describes a saving throw action."
  [text]
  (and (string? text) (re-find #"(?i)saving\s+throw" text)))

(defn damage-message?
  "True when the chat body is an attack or saving throw that may deal damage."
  [text]
  (or (attack-message? text) (saving-throw-message? text)))

(defn parse-damage-expressions
  "Parses parenthesized dice expressions such as (2d6 + 7) from attack or saving throw text."
  [text]
  (when (damage-message? text)
    (vec
     (for [[_ count sides mod] (re-seq #"\((\d+)d(\d+)(?:\s*\+\s*(\d+))?\)" text)]
       {:count (js/parseInt count 10)
        :sides (js/parseInt sides 10)
        :modifier (or (parse-modifier mod) 0)}))))

(defn roll-damage [text]
  (when-let [exprs (seq (parse-damage-expressions text))]
    (mapv
     (fn [{:keys [count sides modifier]}]
       (let [rolls (vec (repeatedly count #(inc (rand-int sides))))
             total (+ (reduce + rolls) modifier)]
         {:count count :sides sides :modifier modifier :rolls rolls :total total}))
     exprs)))

(defn damage-chat-body
  "Rolls damage dice from an attack or saving throw chat message and returns follow-up text."
  [attack-body]
  (when-let [results (seq (roll-damage attack-body))]
    (let [name (or (when (string? attack-body)
                     (some-> (first (str/split attack-body #"\.| — " 2))
                             str/trim))
                   "Action")
          grand-total (reduce + (map :total results))
          parts (mapv
                 (fn [{:keys [count sides modifier rolls total]}]
                   (str count "d" sides
                        (when (pos? modifier) (str "+" modifier))
                        ": " total
                        (when (> count 1)
                          (str " [" (str/join "+" rolls) "]"))))
                 results)]
      (str name " damage — " grand-total " (" (str/join ", " parts) ")"))))

(defn action-chat-body
  "Builds chat text for a sheet action, rolling d20 + modifier for attacks."
  [name description]
  (let [base (if (str/blank? description)
               name
               (str name ". " description))]
    (if-let [roll (attack-roll-result description)]
      (str base " — " (format-attack-roll roll))
      base)))

(defn modifier-from-sheet
  "Returns the initiative modifier for the given character sheet map."
  [sheet]
  (cond
    (get-in sheet [:initiative :bonus])
    (or (parse-modifier (get-in sheet [:initiative :bonus])) 0)

    (number? (:dex sheet))
    (js/Math.floor (/ (- (:dex sheet) 10) 2))

    :else 0))

(defn roll-result
  "Rolls initiative for the given sheet and returns die, modifier, and total."
  [sheet]
  (let [modifier (modifier-from-sheet sheet)
        die (inc (rand-int 20))]
    {:die die :modifier modifier :total (+ die modifier)}))

(defn scene-token-ids-by-image
  "Returns entity ids of scene tokens whose image hash matches."
  [data hash]
  (let [user (ds/entity data [:db/ident :user])
        scene-id (:db/id (:camera/scene (:user/camera user)))
        {tokens :scene/tokens}
        (ds/pull data
                 [{:scene/tokens
                   [:db/id {:token/image [:image/hash]}]}]
                 scene-id)]
    (into []
          (comp (filter #(= hash (get-in % [:token/image :image/hash])))
                (map :db/id))
          (or tokens []))))
