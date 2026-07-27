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

(def ^:private attack-modifier-patterns
  [#"(?i)attack\s+roll[^+\d-]*([+-]\d+)"
   #"(?i)(?:melee|ranged)\s+(?:spell|weapon)\s+attack:[^+\d-]*([+-]\d+)"
   #"(?i)spell\s+attack:[^+\d-]*([+-]\d+)"
   #"(?i)([+-]\d+)\s+to hit"])

(defn attack-modifier
  "Parses the to-hit modifier from an action description, if present."
  [description]
  (when (string? description)
    (some (fn [pattern]
            (when-let [m (re-find pattern description)]
              (parse-modifier (nth m 1))))
          attack-modifier-patterns)))

(defn attack-roll-result
  "Rolls d20 plus the attack modifier parsed from the description."
  [description]
  (when-let [modifier (attack-modifier description)]
    (let [die (inc (rand-int 20))]
      {:die die :modifier modifier :total (+ die modifier)})))

(defn format-attack-roll
  [{:keys [die modifier total]}]
  (str total " (d20 " (format-signed-mod modifier) ")"))

(defn ^:private attack-text? [text]
  (or (re-find #"(?i)attack\s+roll" text)
      (re-find #"(?i)(?:melee|ranged)\s+(?:spell|weapon)\s+attack:" text)
      (re-find #"(?i)spell\s+attack:" text)))

(defn attack-message?
  "True when the chat body looks like a sheet attack action."
  [text]
  (and (string? text) (attack-text? text)))

(defn saving-throw-message?
  "True when the chat body describes a saving throw action."
  [text]
  (and (string? text) (re-find #"(?i)saving\s+throw" text)))

(defn damage-message?
  "True when the chat body is an attack or saving throw that may deal damage."
  [text]
  (or (attack-message? text) (saving-throw-message? text)))

(def ^:private non-damage-type-words
  #{"damage" "plus" "or" "and" "on" "with" "if" "to" "the" "a" "an"
    "each" "taking" "deals" "deal"})

(defn ^:private damage-formula
  [{:keys [count sides modifier]}]
  (str count "d" sides (when (pos? modifier) (str "+" modifier))))

(defn damage-button-label
  "Short label for a chat damage button, e.g. \"1d8+4 bludgeoning\"."
  [{:keys [type] :as expr}]
  (let [formula (damage-formula expr)]
    (if (str/blank? type)
      formula
      (str formula " " (str/lower-case type)))))

(defn parse-damage-expressions
  "Parses parenthesized dice expressions such as (2d6 + 7) from attack or saving throw text.
  When multiple expressions are present (versatile weapons, multi-type hits), each is returned
  separately so the UI can offer one damage button per expression."
  [text]
  (when (damage-message? text)
    (vec
     (for [[_ count sides mod type]
           (re-seq #"\((\d+)d(\d+)(?:\s*\+\s*(\d+))?\)(?:\s+([A-Za-z]+))?" text)]
       (cond-> {:count (js/parseInt count 10)
                :sides (js/parseInt sides 10)
                :modifier (or (parse-modifier mod) 0)}
         (and (some? type)
              (not (contains? non-damage-type-words (str/lower-case type))))
         (assoc :type type))))))

(defn ^:private roll-damage-expression
  [{:keys [count sides modifier] :as expr}]
  (let [rolls (vec (repeatedly count #(inc (rand-int sides))))
        total (+ (reduce + rolls) modifier)]
    (assoc expr :rolls rolls :total total)))

(defn roll-damage
  "Rolls every parsed damage expression in text. Prefer damage-chat-body with an index
  when the caller intends a single button press."
  [text]
  (when-let [exprs (seq (parse-damage-expressions text))]
    (mapv roll-damage-expression exprs)))

(defn ^:private action-name-from-body [attack-body]
  (or (when (string? attack-body)
        (some-> (first (str/split attack-body #"\.| — " 2))
                str/trim))
      "Action"))

(defn structured-damage-expressions
  "Normalizes structured attack damage options into parse-damage-expressions shape."
  [damage-options]
  (when (seq damage-options)
    (mapv (fn [d]
            (cond-> {:count (or (:count d) 1)
                     :sides (or (:sides d) 6)
                     :modifier (or (:modifier d) 0)}
              (:type d) (assoc :type (str (:type d)))
              (:label d) (assoc :label (:label d))
              (:id d) (assoc :id (:id d))))
          damage-options)))

(defn damage-expressions
  "Dual-read: use structured damage options when provided, else parse prose."
  ([text]
   (parse-damage-expressions text))
  ([text structured]
   (or (not-empty (structured-damage-expressions structured))
       (parse-damage-expressions text))))

(defn damage-chat-body
  "Rolls one damage expression from an attack or saving throw chat message.
  index selects which parsed expression to roll (one button → one expression).
  Optional structured damage options prefer v2 attack.damage over prose scan."
  ([attack-body]
   (damage-chat-body attack-body 0 nil))
  ([attack-body index]
   (damage-chat-body attack-body index nil))
  ([attack-body index structured]
   (when-let [exprs (damage-expressions attack-body structured)]
     (when-let [expr (get exprs index)]
       (let [{:keys [count rolls total type] :as result}
             (roll-damage-expression expr)
             name (action-name-from-body attack-body)
             formula (damage-formula result)
             detail (str formula ": " total
                         (when (> count 1)
                           (str " [" (str/join "+" rolls) "]")))
             type-suffix (when-not (str/blank? type)
                           (str " " (str/lower-case type)))]
         (str name " damage" type-suffix " — " total " (" detail ")"))))))

(defn action-chat-body
  "Builds chat text for a sheet action, rolling d20 + modifier for attacks.
  Optional attack-bonus overrides prose parsing (structured v2 attacks)."
  ([name description]
   (action-chat-body name description nil))
  ([name description attack-bonus]
   (let [base (if (str/blank? description)
                name
                (str name ". " description))]
     (if-let [modifier (or attack-bonus (attack-modifier description))]
       (let [die (inc (rand-int 20))
             total (+ die modifier)]
         (str base " — " (format-attack-roll {:die die :modifier modifier :total total})))
       base))))

(defn modifier-from-sheet
  "Returns the initiative modifier for the given character sheet map.
  Dual-read: vitals.initiative.bonus (v2), initiative.bonus (v1), or DEX mod."
  [sheet]
  (cond
    (number? (get-in sheet [:vitals :initiative :bonus]))
    (get-in sheet [:vitals :initiative :bonus])

    (get-in sheet [:vitals :initiative :bonus])
    (or (parse-modifier (get-in sheet [:vitals :initiative :bonus])) 0)

    (get-in sheet [:initiative :bonus])
    (or (parse-modifier (get-in sheet [:initiative :bonus])) 0)

    (number? (get-in sheet [:abilities :dex :score]))
    (js/Math.floor (/ (- (get-in sheet [:abilities :dex :score]) 10) 2))

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
