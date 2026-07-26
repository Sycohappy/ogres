(ns ogres.app.component.character-sheet-editor
  (:require [clojure.string :as str]
            [ogres.app.hooks :as hooks]
            [uix.core :as uix :refer [defui $]]))

(def ^:private abilities [:str :dex :con :int :wis :cha])

(defn ^:private event-value [event]
  (.. event -target -value))

(defn ^:private number-value [value]
  (when-not (str/blank? value)
    (let [number (js/Number value)]
      (when-not (js/isNaN number) number))))

(defn ^:private split-values [value]
  (->> (str/split (or value "") #"[,\n]")
       (map str/trim)
       (remove str/blank?)
       vec))

(defn ^:private join-values [values]
  (str/join ", " (or values [])))

(defui ^:private field
  [{:keys [label value type placeholder on-change min step]}]
  ($ :label.character-editor-field
    ($ :span label)
    ($ :input.text
      (cond-> {:type (or type "text")
               :value (or value "")
               :placeholder placeholder
               :on-change #(on-change (event-value %))}
        (some? min) (assoc :min min)
        (some? step) (assoc :step step)))))

(defui ^:private list-field
  [{:keys [label value placeholder on-change]}]
  ($ :label.character-editor-field.character-editor-field-wide
    ($ :span label)
    ($ :textarea.text.character-editor-textarea
      {:value (join-values value)
       :placeholder placeholder
       :on-change #(on-change (split-values (event-value %)))})))

(defn ^:private map-rows [value]
  (->> (or value {})
       (sort-by (comp name key))
       (mapv (fn [[key value]] {:key (name key) :value value}))))

(defn ^:private rows->map [rows numeric?]
  (into {}
        (keep (fn [{:keys [key value]}]
                (let [key (str/trim key)
                      value (if numeric? (number-value (str value)) (str/trim (str value)))]
                  (when (and (seq key) (some? value))
                    [(keyword key) value]))))
        rows))

(defui ^:private map-editor
  [{:keys [label value numeric? key-placeholder value-placeholder on-change]}]
  (let [rows (map-rows value)
        update-row
        (fn [idx attr next-value]
          (on-change
           (rows->map
            (assoc-in rows [idx attr] next-value)
            numeric?)))
        remove-row
        (fn [idx]
          (on-change (rows->map
                      (vec (concat (subvec rows 0 idx) (subvec rows (inc idx))))
                      numeric?)))]
    ($ :.character-editor-group
      ($ :h4 label)
      (for [[idx row] (map-indexed vector rows)]
        ($ :.character-editor-row
          {:key (str idx "-" (:key row))}
          ($ :input.text
            {:value (:key row)
             :placeholder key-placeholder
             :aria-label (str label " name")
             :on-change #(update-row idx :key (event-value %))})
          ($ :input.text
            {:type (if numeric? "number" "text")
             :value (:value row)
             :placeholder value-placeholder
             :aria-label (str label " value")
             :on-change #(update-row idx :value (event-value %))})
          ($ :button.button.button-danger.character-editor-remove
            {:type "button" :aria-label (str "Remove " label " row")
             :on-click #(remove-row idx)}
            "Remove")))
      ($ :button.button.button-neutral
        {:type "button"
         :on-click #(on-change
                     (rows->map
                      (conj rows {:key (str "new-" (inc (count rows)))
                                  :value (if numeric? 0 "+0")})
                      numeric?))}
        (str "Add " label)))))

(defn ^:private update-entry [entries idx attr value]
  (assoc-in (vec entries) [idx attr] value))

(defn ^:private spell-level-key [level]
  (str level))

(defn ^:private spell-level-sort [level]
  (js/parseInt (spell-level-key level)))

(defn ^:private spell-level-rows [spells]
  (if (seq spells)
    (->> spells
         (sort-by (fn [[k _]] (spell-level-sort k)))
         (mapv (fn [[level data]]
                 {:level (spell-level-key level)
                  :slots (:slots data)
                  :spells (join-values (:spells data))})))
    [{:level "0" :slots nil :spells ""}]))

(defn ^:private spell-level-rows->map [rows]
  (into {}
        (keep (fn [{:keys [level slots spells]}]
                (let [level (str/trim (str level))
                      spells (split-values spells)]
                  (when (and (seq level) (seq spells))
                    [(str level)
                     (cond-> {:spells spells}
                       (and (not= level "0") (some? slots) (not (str/blank? (str slots))))
                       (assoc :slots (number-value (str slots))))])))
              rows)))

(defn ^:private update-spellcasting-block [blocks idx path value]
  (assoc-in (vec blocks) (cons idx path) value))

(defui ^:private spell-levels-editor
  [{:keys [value on-change]}]
  (let [rows (spell-level-rows value)
        update-row
        (fn [idx attr next-value]
          (on-change
           (spell-level-rows->map
            (assoc-in rows [idx attr] next-value))))
        remove-row
        (fn [idx]
          (on-change
           (spell-level-rows->map
            (vec (concat (subvec rows 0 idx) (subvec rows (inc idx)))))))]
    ($ :.character-editor-group
      ($ :h4 "Spell levels")
      (for [[idx {:keys [level slots spells]}] (map-indexed vector rows)]
        ($ :.character-editor-spell-level
          {:key (str idx "-" level)}
          ($ :input.text
            {:type "number" :min 0 :max 9
             :value level
             :placeholder "0"
             :aria-label "Spell level"
             :on-change #(update-row idx :level (event-value %))})
          ($ :input.text
            {:type "number" :min 0
             :value (or slots "")
             :placeholder "Slots"
             :aria-label "Spell slots"
             :disabled (= level "0")
             :on-change #(update-row idx :slots (event-value %))})
          ($ :input.text.character-editor-spell-list
            {:value spells
             :placeholder "Guidance, Cure Wounds"
             :aria-label "Spells"
             :on-change #(update-row idx :spells (event-value %))})
          ($ :button.button.button-danger.character-editor-remove
            {:type "button"
             :on-click #(remove-row idx)}
            "Remove")))
      ($ :button.button.button-neutral
        {:type "button"
         :on-click
         #(on-change
           (spell-level-rows->map
            (conj rows {:level (str (inc (count rows)))
                        :slots nil
                        :spells ""})))}
        "Add spell level"))))

(defn ^:private ensure-spellcasting [spellcasting]
  (if (seq spellcasting)
    (vec spellcasting)
    [{:name "Spellcasting"
      :headerEntries []
      :ability nil
      :spells {}}]))

(defui ^:private spellcasting-meta-editor
  [{:keys [value on-change]}]
  (let [blocks (ensure-spellcasting value)]
    ($ :.character-editor-group.character-editor-spellcasting
      (for [[idx {:keys [name headerEntries ability]}] (map-indexed vector blocks)]
        ($ :.character-editor-spellcasting-block
          {:key idx}
          ($ field {:label "Block name"
                    :value name
                    :placeholder "Spellcasting"
                    :on-change #(on-change (update-spellcasting-block blocks idx [:name] %))})
          ($ field {:label "Ability"
                    :value ability
                    :placeholder "wis"
                    :on-change #(on-change
                                 (update-spellcasting-block blocks idx [:ability]
                                                              (when-not (str/blank? %)
                                                                (keyword (str/lower-case %)))))})
          ($ :label.character-editor-field.character-editor-field-wide
            ($ :span "Header")
            ($ :textarea.text.character-editor-textarea
              {:value (str/join "\n" (or headerEntries []))
               :placeholder "Spell save DC and attack bonus notes"
               :on-change
               #(on-change
                 (update-spellcasting-block
                  blocks idx [:headerEntries]
                  (vec (remove str/blank? (str/split-lines (event-value %))))))}))
          (when (pos? idx)
            ($ :button.button.button-danger.character-editor-remove
              {:type "button"
               :on-click #(on-change
                            (vec (concat (subvec blocks 0 idx)
                                         (subvec blocks (inc idx)))))}
              "Remove spellcasting block"))))
      ($ :button.button.button-neutral
        {:type "button"
         :on-click #(on-change
                     (conj blocks {:name "Spellcasting"
                                   :headerEntries []
                                   :ability nil
                                   :spells {}}))}
        "Add spellcasting block"))))

(defui ^:private entries-editor
  [{:keys [label value on-change]}]
  (let [entries (vec (or value []))]
    ($ :.character-editor-group.character-editor-entries
      ($ :h4 label)
      (for [[idx entry] (map-indexed vector entries)]
        ($ :.character-editor-entry
          {:key idx}
          ($ :input.text
            {:value (or (:name entry) "")
             :placeholder "Name"
             :aria-label (str label " name")
             :on-change #(on-change
                          (update-entry entries idx :name (event-value %)))})
          ($ :textarea.text.character-editor-textarea
            {:value (str/join "\n" (or (:entries entry) []))
             :placeholder "Description"
             :aria-label (str label " description")
             :on-change #(on-change
                          (update-entry entries idx :entries
                                        (->> (str/split-lines (event-value %))
                                             (remove str/blank?)
                                             vec)))})
          ($ :button.button.button-danger.character-editor-remove
            {:type "button"
             :on-click #(on-change
                         (vec (concat (subvec entries 0 idx)
                                      (subvec entries (inc idx)))))}
            "Remove")))
      ($ :button.button.button-neutral
        {:type "button"
         :on-click #(on-change (conj entries {:name "" :entries [""]}))}
        (str "Add " label)))))

(defui sheet-editor
  [{:keys [id sheet on-cancel]}]
  (let [dispatch (hooks/use-dispatch)
        [draft set-draft] (uix/use-state sheet)
        set-field (fn [key value] (set-draft #(assoc % key value)))
        set-nested (fn [path value] (set-draft #(assoc-in % path value)))
        name-valid? (not (str/blank? (:name draft)))]
    ($ :form.character-editor
      {:on-submit
       (fn [event]
         (.preventDefault event)
         (when name-valid?
           (dispatch :character-sheets/update id draft)
           (when on-cancel (on-cancel))))}
      ($ :fieldset.fieldset
        ($ :legend "Identity")
        ($ :.character-editor-grid
          ($ field {:label "Name" :value (:name draft)
                    :on-change #(set-field :name %)})
          ($ list-field {:label "Size" :value (:size draft)
                         :placeholder "M"
                         :on-change #(set-field :size %)})
          ($ field {:label "Type" :value (:type draft)
                    :on-change #(set-field :type %)})
          ($ field {:label "Subtype" :value (:subtype draft)
                    :on-change #(set-field :subtype %)})
          ($ list-field {:label "Alignment" :value (:alignment draft)
                         :placeholder "N, G"
                         :on-change #(set-field :alignment %)})))
      ($ :fieldset.fieldset
        ($ :legend "Combat")
        ($ :.character-editor-grid
          ($ field {:label "Armor Class" :value (join-values (:ac draft))
                    :placeholder "15, 18"
                    :on-change #(set-field :ac
                                           (into [] (keep number-value)
                                                 (split-values %)))})
          ($ field {:label "Hit Points" :type "number"
                    :value (get-in draft [:hp :average])
                    :min 0
                    :on-change #(set-nested [:hp :average] (number-value %))})
          ($ field {:label "Hit Dice" :value (get-in draft [:hp :formula])
                    :placeholder "8d8 + 16"
                    :on-change #(set-nested [:hp :formula] %)})
          ($ field {:label "Initiative Bonus"
                    :value (get-in draft [:initiative :bonus])
                    :placeholder "+2"
                    :on-change #(set-nested [:initiative :bonus] %)})
          ($ field {:label "Passive Perception" :type "number"
                    :value (:passive draft)
                    :on-change #(set-field :passive (number-value %))})
          ($ field {:label "Challenge / Level" :value (:cr draft)
                    :on-change #(set-field :cr %)})
          ($ field {:label "Proficiency Bonus" :value (:proficiency-bonus draft)
                    :placeholder "+2"
                    :on-change #(set-field :proficiency-bonus %)}))
        ($ :.character-editor-abilities
          (for [ability abilities]
            ($ field {:key ability
                      :label (str/upper-case (name ability))
                      :type "number"
                      :value (get draft ability)
                      :on-change #(set-field ability (number-value %))})))
        ($ map-editor {:label "Speed" :value (:speed draft) :numeric? true
                       :key-placeholder "walk" :value-placeholder "30"
                       :on-change #(set-field :speed %)})
        ($ map-editor {:label "Skills" :value (:skill draft)
                       :key-placeholder "perception" :value-placeholder "+4"
                       :on-change #(set-field :skill %)}))
      ($ :fieldset.fieldset
        ($ :legend "Details")
        ($ :.character-editor-grid
          (for [[key label] [[:immune "Immunities"]
                             [:resist "Resistances"]
                             [:vulnerable "Vulnerabilities"]
                             [:senses "Senses"]
                             [:languages "Languages"]
                             [:environment "Environment"]
                             [:treasure "Treasure"]]]
            ($ list-field {:key key :label label :value (get draft key)
                           :on-change #(set-field key %)}))))
      ($ :fieldset.fieldset
        ($ :legend "Spellcasting")
        ($ spellcasting-meta-editor
          {:value (:spellcasting draft)
           :on-change #(set-field :spellcasting %)}))
      ($ :fieldset.fieldset
        ($ :legend "Spells")
        (let [blocks (ensure-spellcasting (:spellcasting draft))]
          ($ :.character-editor-spellcasting
            (for [[idx {:keys [name spells]}] (map-indexed vector blocks)]
              ($ :.character-editor-spellcasting-block
                {:key idx}
                (when (and name (not= name "Spellcasting"))
                  ($ :h4.character-editor-spell-block-title name))
                ($ spell-levels-editor
                  {:value spells
                   :on-change
                   #(set-field :spellcasting
                               (update-spellcasting-block blocks idx [:spells] %))}))))))
      (for [[key label] [[:trait "Traits"]
                         [:action "Actions"]
                         [:bonus "Bonus Actions"]
                         [:reaction "Reactions"]
                         [:legendary "Legendary Actions"]]]
        ($ :fieldset.fieldset {:key key}
          ($ :legend label)
          ($ entries-editor {:label label :value (get draft key)
                             :on-change #(set-field key %)})))
      ($ :.character-editor-actions
        ($ :button.button.button-primary
          {:type "submit" :disabled (not name-valid?)}
          "Save character")
        ($ :button.button.button-neutral
          {:type "button" :on-click on-cancel}
          "Cancel")))))
