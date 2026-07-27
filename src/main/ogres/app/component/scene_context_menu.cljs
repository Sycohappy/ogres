(ns ogres.app.component.scene-context-menu
  (:require [clojure.string :refer [capitalize blank?]]
            [ogres.app.character-sheet :as sheet]
            [ogres.app.component :refer [icon]]
            [ogres.app.component.scene-pattern :refer [pattern]]
            [ogres.app.hooks :as hooks]
            [ogres.app.util :as util]
            [uix.core :as uix :refer [defui $]]))

(defn ^:private token-size [x]
  (cond (<= x 3)  "Tiny"
        (<  x 5)  "Small"
        (<= x 5)  "Medium"
        (<= x 10) "Large"
        (<= x 15) "Huge"
        (>  x 15) "Gargantuan"
        :else     "Unknown"))

(def ^:private token-conditions
  [{:value :blinded       :icon "eye-slash-fill"}
   {:value :charmed       :icon "arrow-through-heart-fill"}
   {:value :defeaned      :icon "ear-fill"}
   {:value :exhausted     :icon "moon-stars-fill"}
   {:value :frightened    :icon "black-cat"}
   {:value :grappled      :icon "fist"}
   {:value :incapacitated :icon "lock-fill"}
   {:value :invisible     :icon "incognito"}
   {:value :paralyzed     :icon "cobra"}
   {:value :petrified     :icon "gem"}
   {:value :poisoned      :icon "poison-bottle"}
   {:value :prone         :icon "tripwire"}
   {:value :restrained    :icon "cobweb"}
   {:value :stunned       :icon "stars"}
   {:value :unconscious   :icon "activity"}])

(def ^:private shape-colors
  ["red"   "orange"  "amber"  "yellow" "lime"
   "green" "emerald" "teal"   "cyan"   "sky"
   "blue"  "indigo"  "violet" "purple" "fuchsia"])

(def ^:private shape-patterns
  [{:value :solid   :label "Fill"}
   {:value :empty   :label "Empty"}
   {:value :lines   :label "Lines"}
   {:value :crosses :label "Crosses"}
   {:value :caps    :label "Caps"}])

(defn ^:private prevent-default [event]
  (.preventDefault event))

(defn ^:private stop-propagation [event]
  (.stopPropagation event))

(defui ^:private action-hide
  [{:keys [value disabled on-change]
    :or   {value false disabled false on-change prevent-default}}]
  ($ :label.context-menu-action
    {:data-tooltip (if value "Reveal" "Hide")}
    ($ :input
      {:type "checkbox"
       :name "hidden"
       :checked value
       :disabled disabled
       :aria-disabled disabled
       :on-change
       (fn [event]
         (on-change (.-checked (.-target event))))})
    ($ icon {:name (if value "eye-slash-fill" "eye-fill")})))

(defui ^:private action-lock
  [{:keys [value disabled on-change]
    :or   {value false disabled false on-change prevent-default}}]
  ($ :label.context-menu-action
    {:data-tooltip (if value "Unlock" "Lock")}
    ($ :input
      {:type "checkbox"
       :name "hidden"
       :checked value
       :disabled disabled
       :aria-disabled disabled
       :on-change
       (fn [event]
         (on-change (.-checked (.-target event))))})
    ($ icon {:name (if value "lock" "unlock")})))

(defui ^:private action-remove
  [{:keys [disabled on-click]
    :or   {disabled false on-click prevent-default}}]
  ($ :button
    {:type "button"
     :disabled disabled
     :data-tooltip "Remove"
     :style {:margin-left "auto"}
     :on-click (fn [] (on-click))}
    ($ icon {:name "trash3-fill"})))

(defui ^:private context-menu-fn
  [{:keys [render-toolbar render-aside children]
    :or   {render-toolbar (constantly nil)
           render-aside   (constantly nil)}}]
  (let [[selected set-selected] (uix/use-state nil)
        props {:selected  selected
               :on-change (fn [form]
                            (if (= selected form)
                              (set-selected nil)
                              (set-selected form)))}]
    ($ :.context-menu {:on-pointer-down stop-propagation}
      ($ :.context-menu-main {:data-expanded (some? selected)}
        ($ :.context-menu-toolbar
          (render-toolbar props))
        (if selected
          ($ :.context-menu-form
            {:class (str "context-menu-form-" (name selected))}
            (children props))))
      ($ :.context-menu-aside
        ($ :.context-menu-toolbar
          (render-aside props))))))

(defui ^:private checkbox
  [{:keys [checked children]}]
  (let [input (uix/use-ref)
        indtr (= checked :indeterminate)]
    (uix/use-effect
     (fn [] (set! (.-indeterminate @input) indtr)) [indtr])
    (children input)))

(defui ^:private token-form-label
  [{:keys [values on-change on-close]
    :or   {values    (constantly (list))
           on-change identity
           on-close  identity}}]
  (let [[dirty set-dirty] (uix/use-state false)
        input (uix/use-ref)
        label (let [vs (values :token/label)]
                (if (= (count vs) 1) (first vs) ""))]
    (uix/use-effect
     (fn [] (.select @input)) [])
    ($ :form
      {:on-submit
       (fn [event]
         (.preventDefault event)
         (let [value (.. event -target -elements -label -value)]
           (on-change :token/change-label value)
           (on-close)))
       :on-blur
       (fn [event]
         (when dirty
           (on-change :token/change-label (.-value (.-target event))))
         (on-close))}
      ($ :input.text.text-ghost
        {:type "text"
         :name "label"
         :placeholder "Change token label"
         :default-value label
         :ref input
         :auto-focus true
         :on-change
         (fn []
           (set-dirty true))}))))

(defui ^:private token-form-details
  [{:keys [on-change values]
    :or   {values (constantly (list)) on-change identity}}]
  (let [value-fn (fn [attr] (first (into (sorted-set-by >) (values attr))))
        sheet (first (values :token/character-sheet))
        hp-cur (sheet/runtime-current-hp sheet)
        hp-max (when sheet (sheet/hp-max sheet))
        ac (when sheet (sheet/ac-value sheet))
        auras (let [xs (first (values :token/auras))
                    legacy (or (value-fn :token/aura-radius) 0)]
                (cond
                  (seq xs) (vec xs)
                  (pos? legacy) [{:id "legacy" :radius legacy :color "teal"}]
                  :else []))]
    ($ :<>
      (when sheet
        ($ :.context-menu-sheet
          ($ :p (str (sheet/sheet-name sheet)
                     (when ac (str " — AC " ac))
                     (when hp-cur
                       (str ", HP " hp-cur
                            (when hp-max (str "/" hp-max))))
                     (when-let [cr (:cr sheet)] (str " (CR " cr ")"))))))
      ($ :.context-menu-form-details-grid
        (let [value (or (value-fn :token/size) 5)]
          ($ :<>
            ($ :label "Size")
            ($ :button
              {:type "button"
               :auto-focus true
               :on-click #(on-change :token/change-size (max (- value 5) 5))
               :aria-label "Decrease token size by 5 feet"}
              "-")
            ($ :data {:value value}
              (str value "ft. " (token-size value)))
            ($ :button
              {:type "button"
               :on-click #(on-change :token/change-size (min (+ value 5) 50))
               :aria-label "Increase token size by 5 feet"} "+")))
        (let [value (or (value-fn :token/light) 0)]
          ($ :<>
            ($ :label "Light")
            ($ :button
              {:type "button"
               :on-click #(on-change :token/change-light (max (- value 5) 0))
               :aria-label "Decrease light radius by 5 feet"}
              "-")
            ($ :data {:value value}
              (if (> value 0) (str value "ft. radius") "None"))
            ($ :button
              {:type "button"
               :on-click #(on-change :token/change-light (min (+ value 5) 120))
               :aria-label "Increase light radius by 5 feet"}
              "+"))))
      ($ :.context-menu-auras
        ($ :strong {:style {:font-size "12px"}} "Auras")
        (for [{:keys [id radius color] :or {color "teal" radius 0}} auras]
          ($ :.context-menu-aura-row {:key (str id)}
            ($ :button
              {:type "button"
               :on-click #(on-change :token/update-aura id {:radius (max (- radius 5) 0)})
               :aria-label "Decrease aura radius"}
              "-")
            ($ :data (if (pos? radius) (str radius "ft.") "None"))
            ($ :button
              {:type "button"
               :on-click #(on-change :token/update-aura id {:radius (min (+ radius 5) 120)})
               :aria-label "Increase aura radius"}
              "+")
            ($ :button
              {:type "button"
               :title "Remove aura"
               :aria-label "Remove aura"
               :on-click #(on-change :token/remove-aura id)}
              ($ icon {:name "x" :size 14}))
            ($ :.context-menu-aura-colors
              (for [c shape-colors]
                ($ :label {:key c :data-color c :title c :aria-label c}
                  ($ :input
                    {:type "radio"
                     :name (str "aura-color-" id)
                     :checked (= c color)
                     :on-change #(on-change :token/update-aura id {:color c})}))))))
        ($ :.context-menu-aura-actions
          ($ :button.button.button-neutral
            {:type "button"
             :style {:font-size "12px" :padding "4px 8px"}
             :on-click #(on-change :token/add-aura)}
            "+ Add aura"))))))

(defui ^:private token-form-sheet
  [{:keys [on-change values]}]
  (let [result (hooks/use-query
                [{:root/character-sheets
                  [:character-sheet/id
                   :character-sheet/name
                   :character-sheet/data]}]
                [:db/ident :root])
        sheets (or (:root/character-sheets result) [])
        current (first (values :token/character-sheet))
        selected-id
        (or (some (fn [{:character-sheet/keys [id data]}]
                    (when (= data current) (str id)))
                  sheets)
            (when (map? current)
              (let [want (sheet/sheet-name current)]
                (some (fn [{:character-sheet/keys [id data]}]
                        (when (= want (sheet/sheet-name data)) (str id)))
                      sheets)))
            "")]
    ($ :<>
      ($ :label {:style {:justify-content "flex-start"}} "Link sheet")
      ($ :select.text
        {:value (or selected-id "")
         :style {:grid-column "1 / -1" :width "100%"}
         :on-change
         (fn [event]
           (let [id (.. event -target -value)]
             (if (blank? id)
               (on-change :token/change-character-sheet nil)
               (let [entry (first (filter #(= id (str (:character-sheet/id %))) sheets))]
                 (on-change :token/change-character-sheet
                            (:character-sheet/data entry))))))}
        ($ :option {:value ""} "No character sheet")
        (for [{:character-sheet/keys [id name data]} sheets]
          ($ :option {:key (str id) :value (str id)}
            (str name
                 (when (sheet/pc-sheet? data) " (PC)")
                 (when (:cr data) (str " · CR " (:cr data)))))))
      (when current
        ($ :p.context-menu-sheet-hint
          {:style {:grid-column "1 / -1" :margin 0 :opacity 0.85 :font-size "12px"}}
          (str "HP "
               (or (sheet/runtime-current-hp current) "?")
               "/"
               (or (sheet/hp-max current) "?")
               " · AC "
               (or (sheet/ac-value current) "?")
               " · linking marks PC tokens and syncs initiative HP"))))))

(defui ^:private token-form-conditions
  [props]
  (let [fqs (frequencies (reduce into [] ((:values props) :token/flags [])))
        ids ((:values props) :db/id)]
    (for [{value :value icon-name :icon} token-conditions
          :let [focus (= value (:value (first token-conditions)))
                state (cond (= (get fqs value 0) 0) false
                            (= (get fqs value 0) (count ids)) true
                            :else :indeterminate)]]
      ($ checkbox {:key value :checked state}
        (fn [input]
          ($ :label {:aria-label (name value) :data-tooltip (capitalize (name value))}
            ($ :input
              {:ref input
               :type "checkbox"
               :name (str "token-condition-" (name value))
               :checked (if (= state :indeterminate) false state)
               :auto-focus focus
               :on-change
               (fn [event]
                 (let [checked (.. event -target -checked)]
                   ((:on-change props) :token/change-flag value checked)))})
            ($ icon {:name icon-name})))))))

(defui ^:private context-menu-token [props]
  (let [dispatch (hooks/use-dispatch)
        publish  (hooks/use-publish)
        data     (:data props)
        idxs     (into [] (map :db/id) data)
        sheets-q (hooks/use-query
                  [{:root/character-sheets
                    [:character-sheet/id :character-sheet/name :character-sheet/data]}]
                  [:db/ident :root])
        library  (or (:root/character-sheets sheets-q) [])
        sheet    (:token/character-sheet (first data))
        hash     (:image/hash (:token/image (first data)))
        sheet-id (when (map? sheet)
                   (or (some (fn [{:character-sheet/keys [id data]}]
                               (when (= data sheet) (str id)))
                             library)
                       (let [want (sheet/sheet-name sheet)]
                         (some (fn [{:character-sheet/keys [id data]}]
                                 (when (= want (sheet/sheet-name data)) (str id)))
                               library))
                       ""))]
    ($ context-menu-fn
      {:render-toolbar
       (fn [{:keys [selected on-change]}]
         ($ :<>
           (for [[form icon-name tooltip]
                 [[:label "fonts" "Label"]
                  [:details "sliders" "Options"]
                  [:sheet "person-circle" "Link character sheet"]
                  [:conditions "arrow-through-heart-fill" "Conditions"]]]
             ($ :button
               {:key form
                :type "button"
                :data-selected (= selected form)
                :data-tooltip tooltip
                :on-click #(on-change form)}
               ($ icon {:name icon-name})))
           ($ :button
             {:type "button"
              :data-tooltip "Pop out character sheet"
              :disabled (nil? sheet)
              :on-click
              #(when sheet
                 (publish :character-sheets/open-popout sheet (or hash "") (or sheet-id "")))}
             ($ icon {:name "pip"}))
           (let [on (every? (comp vector? :scene/_initiative) data)]
             ($ :<>
               ($ :button
                 {:type "button"
                  :data-selected on
                  :data-tooltip "Initiative"
                  :on-click #(dispatch :initiative/toggle idxs (not on))}
                 ($ icon {:name "hourglass-split"}))
               ($ :button
                 {:type "button"
                  :data-tooltip "Roll initiative"
                  :on-click #(doseq [{id :db/id} data] (dispatch :initiative/roll id))}
                 ($ icon {:name "dice-5-fill"}))))
           (let [on (every? (comp boolean :player :token/flags) data)]
             ($ :button
               {:type "button"
                :data-tooltip "PC / Player"
                :data-selected on
                :on-click #(dispatch :token/change-flag idxs :player (not on))}
               ($ icon {:name "people-fill"})))
           (let [on (every? (comp boolean :dead :token/flags) data)]
             ($ :button
               {:type "button"
                :data-tooltip "Dead"
                :data-selected on
                :on-click #(dispatch :token/change-dead idxs (not on))}
               ($ icon {:name "skull"})))
           (let [xfr (comp :token-image/url :token/image)
                 url (js/URL.parse (xfr (first data)))]
             (if (and (some? url)
                      (util/uniform-by xfr data)
                      (or (:host props)
                          (every? (comp :image/public :token/image) data)))
               ($ :a {:href (.-href url) :target "_blank" :data-tooltip "Open link"}
                 ($ icon {:name "box-arrow-up-right"}))))))
       :render-aside
       (fn []
         ($ :<>
           ($ action-hide
             {:value (every? :object/hidden data)
              :disabled (not (:host props))
              :on-change
              (fn []
                (dispatch :objects/toggle-hidden-selected))})
           ($ action-remove
             {:on-click
              (fn []
                (dispatch :objects/remove-selected))})))}
      (fn [{:keys [selected on-change]}]
        (let [props {:on-close  #(on-change nil)
                     :on-change #(apply dispatch %1 idxs %&)
                     :values    (fn vs
                                  ([f] (vs f #{}))
                                  ([f init] (into init (map f) data)))}]
          (case selected
            :label      ($ token-form-label props)
            :details    ($ token-form-details props)
            :sheet      ($ token-form-sheet props)
            :conditions ($ token-form-conditions props)))))))

(defui ^:private shape-form-style
  [{:keys [on-change values]}]
  ($ :.context-menu-form-styles
    (for [{:keys [value label]} shape-patterns]
      (let [id (str "template-pattern-" (name value))]
        ($ :label {:key value :aria-label label}
          ($ :input
            {:type "radio"
             :name "shape-pattern"
             :value value
             :checked (= value (first (values :shape/pattern)))
             :on-change
             (fn [event]
               (let [value (.. event -target -value)]
                 (on-change :objects/update :shape/pattern (keyword value))))})
          ($ :svg
            ($ :defs ($ pattern {:id id :name value}))
            ($ :rect {:width "100%" :height "100%" :fill (str "url(#" id ")")})))))
    (for [value shape-colors]
      ($ :label {:key value :aria-label value :data-color value}
        ($ :input
          {:type "radio"
           :name "shape-color"
           :value value
           :checked (= value (first (values :shape/color)))
           :on-change
           (fn [event]
             (on-change :objects/update :shape/color (.. event -target -value)))})))))

(defui ^:private context-menu-shape [props]
  (let [dispatch (hooks/use-dispatch)
        data     (:data props)]
    ($ context-menu-fn
      {:render-toolbar
       (fn [{:keys [selected on-change]}]
         ($ :button
           {:type "button"
            :data-selected (= selected :style)
            :data-tooltip "Style"
            :on-click #(on-change :style)}
           ($ icon {:name "palette-fill"})))
       :render-aside
       (fn []
         ($ :<>
           ($ action-lock
             {:value (every? :object/locked data)
              :disabled (not (:host props))
              :on-change
              (fn []
                (dispatch :objects/toggle-locked-selected))})
           ($ action-remove
             {:on-click
              (fn []
                (dispatch :objects/remove-selected))})))}
      (fn [{:keys [selected]}]
        (if (= selected :style)
          ($ shape-form-style
            {:values
             (fn vs
               ([f] (vs f #{}))
               ([f init] (into init (map f) data)))
             :on-change
             (fn [event & args]
               (apply dispatch event (map :db/id data) args))}))))))

(defui context-menu-prop [props]
  (let [dispatch (hooks/use-dispatch)
        data     (:data props)]
    ($ context-menu-fn
      {:render-toolbar
       (fn []
         ($ :button
           {:type "button"
            :data-tooltip "Reset size/rotation"
            :on-click
            (fn []
              (dispatch :objects/reset-transform-selected))}
           ($ icon {:name "arrows-angle-expand"})))
       :render-aside
       (fn []
         ($ :<>
           ($ action-hide
             {:value (every? :object/hidden data)
              :disabled (not (:host props))
              :on-change
              (fn []
                (dispatch :objects/toggle-hidden-selected))})
           ($ action-lock
             {:value (every? :object/locked data)
              :disabled (not (:host props))
              :on-change
              (fn []
                (dispatch :objects/toggle-locked-selected))})
           ($ action-remove
             {:on-click
              (fn []
                (dispatch :objects/remove-selected))})))}
      (fn [{:keys []}]))))

(defui context-menu [props]
  (if (util/uniform-by (comp namespace :object/type) (:data props))
    (case (namespace (:object/type (first (:data props))))
      "prop"  ($ context-menu-prop  props)
      "shape" ($ context-menu-shape props)
      "token" ($ context-menu-token props)
      nil)))
