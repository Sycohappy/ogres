(ns ogres.app.component.panel-characters
  (:require [clojure.string :as str]
            [ogres.app.component.character-sheet-editor :refer [sheet-editor]]
            [datascript.core :as ds]
            [ogres.app.hooks :as hooks]
            [ogres.app.initiative :as initiative]
            [ogres.app.provider.state :as state]
            [uix.core :as uix :refer [defui $]]))

(def ^:private query
  [{:root/user [:user/host]}
   {:root/token-images
    [:image/hash
     :image/name
     :image/public
     :token-image/default-label
     :token-image/default-size
     :token-image/default-light
     :token-image/character-sheet]}
   {:root/character-sheets
    [:character-sheet/id
     :character-sheet/name
     :character-sheet/source
     :character-sheet/data]}])

(def ^:private popups (atom {}))

(defn ^:private label-for-token [token]
  (or (:token-image/default-label token)
      (:image/name token)
      (:image/hash token)
      "Unnamed token"))

(defn ^:private label-for-sheet [sheet]
  (str (:name sheet)
       (when-let [cr (:cr sheet)]
         (str " (CR " cr ")"))))

(defn ^:private escape-html [text]
  (str/escape (str text) {\& "&amp;" \< "&lt;" \> "&gt;" \" "&quot;" \' "&#39;"}))

(defn ^:private escape-js-string [text]
  (str/escape (str text) {\" "\\\"" \' "\\'" \newline "\\n" \return "\\r"}))

(defn ^:private maybe-text [x]
  (when (and (some? x) (not= x ""))
    (str x)))

(defn ^:private format-list [x]
  (cond
    (nil? x) nil
    (string? x) x
    (sequential? x) (str/join ", " (map str x))
    :else (str x)))

(defn ^:private format-speed [speed]
  (when (map? speed)
    (->> speed
         (map (fn [[k v]]
                (str (name k) " " v " ft.")))
         (str/join ", "))))

(defn ^:private format-initiative-bonus [sheet]
  (let [initiative (:initiative sheet)]
    (cond
      (and (map? initiative) (:bonus initiative)) (:bonus initiative)
      (number? (:dex sheet))
      (let [mod (initiative/modifier-from-sheet sheet)]
        (str (if (pos? mod) "+" "") mod))
      :else "+0")))

(defn ^:private format-skills [skills]
  (when (and (map? skills) (seq skills))
    (->> skills
         (map (fn [[k v]]
                (str (str/capitalize (name k)) " " v)))
         (str/join ", "))))

(defn ^:private ability-mod [score]
  (let [mod (js/Math.floor (/ (- score 10) 2))
        sign (if (neg? mod) "" "+")]
    (str sign mod)))

(defn ^:private render-stat [label value]
  (when-let [text (maybe-text value)]
    (str "<div class=\"stat\"><span class=\"stat-label\">"
         (escape-html label)
         "</span><span class=\"stat-value\">"
         (escape-html text)
         "</span></div>")))

(defn ^:private render-info-row [label value]
  (when-let [text (maybe-text value)]
    (str "<div class=\"info-row\"><span class=\"info-label\">"
         (escape-html label)
         "</span><span class=\"info-value\">"
         (escape-html text)
         "</span></div>")))

(defn ^:private render-entries [title entries]
  (when (seq entries)
    (str "<section class=\"section\"><h2>"
         (escape-html title)
         "</h2>"
         (apply str
                (for [{:keys [name entries]} entries]
                  (str "<p class=\"entry\"><strong>"
                       (escape-html name)
                       ".</strong> "
                       (escape-html (str/join " " entries))
                       "</p>")))
         "</section>")))

(def ^:private spell-ordinals
  {"1" "1st" "2" "2nd" "3" "3rd" "4" "4th" "5" "5th"
   "6" "6th" "7" "7th" "8" "8th" "9" "9th"})

(defn ^:private spell-level-label [level]
  (let [level (str level)]
    (cond
      (= "0" level) "Cantrips (at will)"
      (contains? spell-ordinals level) (str (get spell-ordinals level) " level")
      :else (str "Level " level))))

(defn ^:private spell-level-key [level]
  (str level))

(defn ^:private spell-level-sort [level]
  (js/parseInt (spell-level-key level)))

(defn ^:private spell-slots-label [slots]
  (when (some? slots)
    (str " (" slots " slot" (when (not= 1 slots) "s") ")")))

(defn ^:private render-spell-level [level data]
  (when-let [spells (seq (:spells data))]
    (str "<p class=\"spell-level\"><strong>"
         (escape-html (spell-level-label level))
         (escape-html (spell-slots-label (:slots data)))
         ":</strong> "
         (escape-html (str/join ", " spells))
         "</p>")))

(defn ^:private traits-without-spells [traits]
  (vec (remove #(= "Spells" (:name %)) (or traits []))))

(defn ^:private spell-trait-text [traits]
  (some (fn [{:keys [name entries]}]
          (when (= "Spells" name)
            (str/join " " entries)))
        (or traits [])))

(defn ^:private render-spellcasting-intro [spellcasting]
  (when (some #(or (seq (:headerEntries %)) (:ability %)) spellcasting)
    (str "<section class=\"section spellcasting\">"
         "<h2>Spellcasting</h2>"
         (apply str
                (for [{:keys [name headerEntries ability]} spellcasting
                      :when (or (seq headerEntries) ability)]
                  (str (when (and name (not= name "Spellcasting"))
                         (str "<h3 class=\"spell-block-title\">"
                              (escape-html name)
                              "</h3>"))
                       (when ability
                         (str "<p class=\"spell-ability\"><em>Spellcasting ability: "
                              (escape-html (str/upper-case (name ability)))
                              "</em></p>"))
                       (apply str
                              (for [entry (or headerEntries [])]
                                (str "<p class=\"entry\">"
                                     (escape-html entry)
                                     "</p>"))))))
         "</section>")))

(defn ^:private render-spell-levels [spells]
  (when (seq spells)
    (apply str
           (for [[level data] (sort-by (fn [[k _]] (spell-level-sort k)) spells)]
             (render-spell-level (spell-level-key level) data)))))

(defn ^:private render-spells-section [spellcasting trait-spells]
  (let [levels (apply str
                      (for [{:keys [name spells]} spellcasting
                            :when (seq spells)]
                        (str (when (and name (not= name "Spellcasting"))
                               (str "<h3 class=\"spell-block-title\">"
                                    (escape-html name)
                                    "</h3>"))
                             (render-spell-levels spells))))
        trait-text (some-> trait-spells str/trim seq)]
    (when (or (seq levels) trait-text)
      (str "<section class=\"section spells\">"
           "<h2>Spells</h2>"
           levels
           (when trait-text
             (str "<p class=\"spell-level\">"
                  (escape-html trait-text)
                  "</p>"))
           "</section>"))))

(defn ^:private render-spell-sections [sheet]
  (let [spellcasting (or (:spellcasting sheet) [])
        trait-spells (spell-trait-text (:trait sheet))]
    (str (or (render-spellcasting-intro spellcasting) "")
         (or (render-spells-section spellcasting trait-spells) ""))))

(defn ^:private action-chat-body [name description]
  (initiative/action-chat-body name description))

(defn ^:private log-chat-action! [action payload]
  (.log js/console (str "[ogres:chat-action] " action) (clj->js payload)))

(defn ^:private action-payload [entries]
  (mapv (fn [{:keys [name entries]}]
          {:name name :description (str/join " " entries)})
        entries))

(defn ^:private render-action-button [idx {:keys [name entries]}]
  (let [description (str/join " " entries)
        attack? (some? (initiative/attack-modifier description))]
    (str "<button type=\"button\" class=\"entry entry-action\" "
         "onclick=\"ogresPostAction(" idx ")\" "
         "title=\"" (if attack? "Roll d20 + modifier and send to chat" "Send to chat") "\">"
         "<strong>" (escape-html name) ".</strong> "
         (escape-html description)
         "</button>")))

(defn ^:private render-clickable-entry-section [title entries start-idx]
  (when (seq entries)
    (str "<section class=\"section\"><h2>"
         (escape-html title)
         "</h2>"
         (apply str (map-indexed (fn [idx entry]
                                   (render-action-button (+ start-idx idx) entry))
                                 entries))
         "</section>")))

(defn ^:private render-action-section [entries]
  (render-clickable-entry-section "Actions" entries 0))

(defn ^:private render-actions-handler [entries popup-id]
  (when (seq entries)
    (let [json (.stringify js/JSON (clj->js (action-payload entries)))]
      (str "<script>"
           "window.ogresPopupId=\"" (escape-js-string popup-id) "\";"
           "window.ogresActions=" json ";"
           "window.ogresPostAction=function(idx){"
           "var popupId=window.ogresPopupId;"
           "var action=window.ogresActions[idx];"
           "console.log(\"[ogres:chat-action] popout click\",{popupId:popupId,idx:idx,action:action,hasOpener:!!window.opener});"
           "if(!action){console.warn(\"[ogres:chat-action] popout missing action\",idx);return;}"
           "if(!window.opener){console.warn(\"[ogres:chat-action] popout no opener\");return;}"
           "var msg={type:\"ogres:chat-action\",popupId:popupId,name:action.name,description:action.description};"
           "console.log(\"[ogres:chat-action] popout postMessage\",msg);"
           "window.opener.postMessage(msg,window.location.origin);"
           "};"
           "console.log(\"[ogres:chat-action] popout init\",{popupId:window.ogresPopupId,actionCount:window.ogresActions.length});"
           "</script>"))))

(defn ^:private render-ability-roll-handler [popup-id]
  (str "<script>"
       "(function(){"
       "var popupId=\"" (escape-js-string popup-id) "\";"
       "window.ogresPostAbilityRoll=function(ability,score){"
       "if(!window.opener){return;}"
       "window.opener.postMessage("
       "{type:\"ogres:ability-roll\",popupId:popupId,ability:ability,score:score},"
       "window.location.origin);"
       "};"
       "})();"
       "</script>"))

(defn ^:private render-initiative-stat [sheet image-hash popup-id]
  (let [bonus (format-initiative-bonus sheet)]
    (str "<div class=\"stat stat-initiative\">"
         "<span class=\"stat-label\">Initiative</span>"
         "<span class=\"stat-value initiative-value\">"
         "<span class=\"initiative-bonus\">" (escape-html bonus) "</span>"
         "<button type=\"button\" class=\"initiative-roll-btn\" aria-label=\"Roll initiative\" title=\"Roll d20 + initiative\">"
         "&#9856;"
         "</button>"
         "<span class=\"initiative-result\"></span>"
         "</span></div>"
         "<script>"
         "(function(){"
         "var hash=\"" (escape-js-string image-hash) "\";"
         "var popupId=\"" (escape-js-string popup-id) "\";"
         "var origin=window.location.origin;"
         "var btn=document.querySelector(\".initiative-roll-btn\");"
         "var result=document.querySelector(\".initiative-result\");"
         "btn.addEventListener(\"click\",function(){"
         "if(!window.opener){result.textContent=\"Cannot reach main window\";return;}"
         "window.opener.postMessage({type:\"ogres:initiative-roll\",hash:hash,popupId:popupId},origin);"
         "});"
         "window.addEventListener(\"message\",function(event){"
         "if(event.origin!==origin)return;"
         "var data=event.data;"
         "if(!data||data.type!==\"ogres:initiative-roll-result\"||data.popupId!==popupId)return;"
         "if(data.error){result.textContent=data.error;return;}"
         "var modStr=data.modifier>=0?\"+\"+data.modifier:String(data.modifier);"
         "result.textContent=data.total+\" (\"+data.die+\" \"+modStr+\")\""
         "+(data.count>1?\" ×\"+data.count:\"\");"
         "});"
         "})();"
         "</script>")))

(defn ^:private render-sheet-html [sheet image-hash popup-id]
  (let [title (:name sheet)
        ac (format-list (:ac sheet))
        hp (:hp sheet)
        hp-text (when (map? hp) (str (:average hp) " (" (:formula hp) ")"))
        abilities (for [ability [:str :dex :con :int :wis :cha]
                        :let [score (get sheet ability)]
                        :when (number? score)]
                    (let [label (str/upper-case (name ability))]
                      (str "<button type=\"button\" class=\"ability\""
                           " onclick=\"ogresPostAbilityRoll('" (escape-js-string label) "'," score ")\""
                           " title=\"Roll d20 " (ability-mod score) "\">"
                           "<span class=\"ability-name\">" label "</span>"
                           "<span class=\"ability-score\">" score "</span>"
                           "<span class=\"ability-mod\">(" (ability-mod score) ")</span>"
                           "</button>")))
        actions (or (:action sheet) [])
        bonus (or (:bonus sheet) [])
        reactions (or (:reaction sheet) [])
        clickable (into [] (concat actions bonus reactions))
        bonus-offset (count actions)
        reaction-offset (+ bonus-offset (count bonus))]
    (str "<!doctype html><html><head><meta charset=\"utf-8\"/>"
         "<title>" (escape-html title) "</title>"
         "<style>"
         "body{font-family:Georgia,serif;margin:0;padding:20px;background:#f5f0e6;color:#1a1a1a;line-height:1.5}"
         "h1{margin:0 0 16px;font-size:28px;font-weight:700;border-bottom:2px solid #8b4513;padding-bottom:8px}"
         ".core-stats{display:grid;grid-template-columns:repeat(4,minmax(120px,1fr));gap:12px;margin-bottom:16px}"
         ".stat{background:#fff;border:1px solid #c4b59a;border-radius:6px;padding:10px;text-align:center}"
         ".stat-label{display:block;font-size:11px;font-weight:700;text-transform:uppercase;color:#666;margin-bottom:4px}"
         ".stat-value{display:block;font-size:18px;font-weight:700}"
         ".stat-initiative .initiative-value{display:flex;align-items:center;justify-content:center;gap:6px;flex-wrap:wrap}"
         ".initiative-bonus{font-size:18px;font-weight:700}"
         ".initiative-roll-btn{background:#f5f0e6;border:1px dotted #8b4513;border-radius:4px;color:#8b4513;cursor:pointer;font-size:18px;line-height:1;padding:2px 6px}"
         ".initiative-roll-btn:hover{background:#e8dcc8}"
         ".initiative-result{font-size:14px;font-weight:700;color:#444;width:100%}"
         ".abilities{display:grid;grid-template-columns:repeat(6,minmax(70px,1fr));gap:8px;margin-bottom:16px}"
         ".ability{background:#fff;border:1px solid #c4b59a;border-radius:6px;padding:8px;text-align:center;cursor:pointer;font-family:inherit;color:inherit;line-height:inherit;width:100%}"
         ".ability:hover{background:#e8dcc8;border-color:#8b4513}"
         ".ability-name{display:block;font-size:11px;font-weight:700;color:#666}"
         ".ability-score{display:block;font-size:20px;font-weight:700;margin:2px 0}"
         ".ability-mod{display:block;font-size:13px;color:#444}"
         ".info-block{background:#fff;border:1px solid #c4b59a;border-radius:6px;padding:12px 16px;margin-bottom:16px}"
         ".info-row{display:flex;gap:8px;padding:3px 0;border-bottom:1px solid #eee}"
         ".info-row:last-child{border-bottom:none}"
         ".info-label{font-weight:700;min-width:140px;color:#555}"
         ".info-value{flex:1;word-break:break-word}"
         ".section{margin-bottom:16px}"
         "h2{margin:0 0 8px;font-size:16px;font-weight:700;border-bottom:1px solid #c4b59a;padding-bottom:4px}"
         ".entry{margin:0 0 8px;font-size:14px}"
         ".spell-level{margin:0 0 8px;font-size:14px}"
         ".spell-ability{margin:0 0 8px;font-size:13px;color:#555}"
         ".spell-block-title{margin:12px 0 6px;font-size:14px;font-weight:700}"
         ".section.spells{margin-top:16px}"
         ".entry-action{display:block;width:100%;text-align:left;background:transparent;border:1px solid transparent;border-radius:4px;padding:6px 8px;margin:0 -8px 8px;cursor:pointer;font:inherit;color:inherit;line-height:inherit}"
         ".entry-action:hover{background:#e8dcc8;border-color:#c4b59a}"
         "details{margin-top:16px;font-size:13px}"
         "pre{background:#222;color:#eee;padding:12px;border-radius:6px;overflow:auto;white-space:pre-wrap;font-size:12px}"
         "</style></head><body>"
         "<h1>" (escape-html title) "</h1>"
         "<div class=\"core-stats\">"
         (or (render-stat "Armor Class" ac) "")
         (or (render-stat "Hit Points" hp-text) "")
         (or (render-stat "Speed" (format-speed (:speed sheet))) "")
         (render-initiative-stat sheet image-hash popup-id)
         "</div>"
         (when (seq abilities)
           (str "<div class=\"abilities\">" (apply str abilities) "</div>"))
         "<div class=\"info-block\">"
         (or (render-info-row "Skills" (format-skills (:skill sheet))) "")
         (or (render-info-row "Immunities" (format-list (:immune sheet))) "")
         (or (render-info-row "Senses" (format-list (:senses sheet))) "")
         (or (render-info-row "Languages" (format-list (:languages sheet))) "")
         (or (render-info-row "Challenge Rating" (:cr sheet)) "")
         (or (render-info-row "Proficiency Bonus" (:proficiency-bonus sheet)) "")
         "</div>"
         (or (render-entries "Traits" (traits-without-spells (:trait sheet))) "")
         (or (render-spell-sections sheet) "")
         (or (render-action-section actions) "")
         (or (render-clickable-entry-section "Bonus Actions" bonus bonus-offset) "")
         (or (render-clickable-entry-section "Reactions" reactions reaction-offset) "")
         (or (render-entries "Legendary Actions" (:legendary sheet)) "")
         "<details><summary>Raw JSON</summary><pre>"
         (escape-html (js/JSON.stringify (clj->js sheet) nil 2))
         "</pre></details>"
         (or (render-actions-handler clickable popup-id) "")
         (render-ability-roll-handler popup-id)
         "</body></html>")))

(defn ^:private open-sheet-popout! [sheet image-hash]
  (let [popup-id (str (random-uuid))
        actions (count (or (:action sheet) []))
        bonus (count (or (:bonus sheet) []))
        reactions (count (or (:reaction sheet) []))
        body (render-sheet-html sheet image-hash popup-id)
        popup (.open js/window "" "_blank" "popup,width=720,height=900")]
    (log-chat-action! "open popout"
                      {:popupId popup-id
                       :sheetName (:name sheet)
                       :imageHash image-hash
                       :actionCount (+ actions bonus reactions)
                       :popupBlocked (nil? popup)})
    (when popup
      (swap! popups assoc popup-id popup)
      (.open (.-document popup))
      (.write (.-document popup) body)
      (.close (.-document popup))
      (.focus popup))))

(defn ^:private reply-to-popout! [popup-id payload]
  (when-let [popup (get @popups popup-id)]
    (when-not (.-closed popup)
      (.postMessage popup (clj->js payload) js/window.location.origin))))

(defui initiative-popout-listeners []
  (let [conn     (uix/use-context state/context)
        dispatch (hooks/use-dispatch)]
    (uix/use-effect
     (fn []
       (let [origin js/window.location.origin
             handler
             (fn [^js event]
               (when (= (.-origin event) origin)
                 (let [data (.-data event)]
                   (case (.-type data)
                     "ogres:initiative-roll"
                     (let [hash (.-hash data)
                           popup-id (.-popupId data)
                           db @conn
                           ids (initiative/scene-token-ids-by-image db hash)
                           sheet (:token-image/character-sheet (ds/entity db [:image/hash hash]))]
                       (if (empty? ids)
                         (reply-to-popout!
                          popup-id
                          {:type "ogres:initiative-roll-result"
                           :popupId popup-id
                           :error "No tokens on scene for this image"})
                         (let [result (initiative/roll-result sheet)]
                           (dispatch :initiative/roll-shared ids sheet result)
                           (reply-to-popout!
                            popup-id
                            (merge {:type "ogres:initiative-roll-result"
                                    :popupId popup-id
                                    :count (count ids)}
                                   result)))))

                     "ogres:chat-action"
                     (let [popup-id (.-popupId data)
                           name (.-name data)
                           description (.-description data)
                           modifier (initiative/attack-modifier description)
                           body (action-chat-body name description)
                           user (ds/entity @conn [:db/ident :user])
                           status (:session/status user)]
                       (log-chat-action! "received"
                                         {:popupId popup-id
                                          :name name
                                          :description description
                                          :attackModifier modifier
                                          :body body
                                          :rolled? (some? modifier)
                                          :session/status status})
                       (if (= status :connected)
                         (do
                           (log-chat-action! "dispatch :chat/send" {:body body})
                           (dispatch :chat/send (random-uuid) body nil (js/Date.now)))
                         (do
                           (log-chat-action! "not connected" {:popupId popup-id})
                           (reply-to-popout!
                            popup-id
                            {:type "ogres:chat-action-result"
                             :popupId popup-id
                             :error "Connect to a session to chat"}))))

                     "ogres:ability-roll"
                     (let [ability (.-ability data)
                           score   (.-score data)
                           user    (ds/entity @conn [:db/ident :user])
                           status  (:session/status user)]
                       (when (= status :connected)
                         (let [mod-n   (js/Math.floor (/ (- score 10) 2))
                               die     (inc (rand-int 20))
                               total   (+ die mod-n)
                               mod-str (if (neg? mod-n) (str mod-n) (str "+" mod-n))
                               body    (str ability " — " total " (d20 " mod-str ")")]
                           (dispatch :chat/send (random-uuid) body nil (js/Date.now)))))

                     nil))))]

         (.addEventListener js/window "message" handler)
         (fn [] (.removeEventListener js/window "message" handler))))
     [conn dispatch])
    nil))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query query [:db/ident :root])
        sheets   (:root/character-sheets result)
        host?    (get-in result [:root/user :user/host])
        tokens   (filter #(or host? (:image/public %)) (:root/token-images result))
        import!  (hooks/use-document-importer)
        import-input (uix/use-ref nil)
        [editing-id set-editing-id] (uix/use-state nil)
        [import-error set-import-error] (uix/use-state nil)
        [import-success set-import-success] (uix/use-state nil)]
    (hooks/use-subscribe
     :import/error
     (uix/use-callback
      (fn [message filename]
        (set-import-success nil)
        (set-import-error (str "Failed to import " filename ": " message))) []))
    (hooks/use-subscribe
     :import/success
     (uix/use-callback
      (fn [import-result]
        (let [names (when (map? import-result) (:names import-result))
              count (if (map? import-result) (:count import-result) import-result)]
          (set-import-error nil)
          (set-import-success
           (if (seq names)
             (str "Imported " count " sheet(s): " (str/join ", " names))
             (str "Imported " count " sheet(s)."))))) []))
    ($ :.form-help
      ($ :header ($ :h2 "Characters"))
      ($ :fieldset.fieldset.character-management-import
        ($ :legend "Add Characters")
        ($ :div.form-notice
          ($ :p
            "Import character sheets from PDF, Markdown, or JSON, or create one from scratch.")
          ($ :.character-management-actions
            ($ :button.button.button-neutral
              {:type "button"
               :on-click #(.. import-input -current (click))}
              "Import sheets")
            ($ :button.button.button-neutral
              {:type "button"
               :on-click #(dispatch :character-sheets/create-blank)}
              "New blank character"))
          ($ :input
            {:type "file"
             :hidden true
             :accept ".pdf,.md,.markdown,.json"
             :multiple true
             :ref import-input
             :on-change
             (fn [event]
               (let [files (.. event -target -files)]
                 (when (seq files)
                   (import! files)
                   (set! (.. event -target -value) ""))))})
          (when import-error
            ($ :p.character-management-message
              {:data-status "error"} import-error))
          (when import-success
            ($ :p.character-management-message
              {:data-status "success"} import-success))))
      ($ :fieldset.fieldset
        ($ :legend "Character Sheets")
        (if (empty? sheets)
          ($ :p.form-notice
            "No character sheets yet. Import a file or create a blank character.")
          ($ :ul.character-sheet-list
            (for [{:character-sheet/keys [id name source data] :as entry} sheets
                  :let [linked-hash
                        (some (fn [token]
                                (when (= data (:token-image/character-sheet token))
                                  (:image/hash token)))
                              tokens)
                        editing? (= id editing-id)]]
              ($ :li.character-sheet-list-item
                {:key id}
                ($ :.character-sheet-summary
                  ($ :div
                    ($ :strong name)
                    ($ :p
                      (str (when (:cr data) (str "CR/Level " (:cr data)))
                           (when (and (:cr data) source) " · ")
                           (or source ""))))
                  ($ :.character-management-actions
                    ($ :button.button.button-neutral
                      {:type "button"
                       :on-click #(set-editing-id (when-not editing? id))}
                      (if editing? "Close editor" "Edit"))
                    ($ :button.button.button-neutral
                      {:type "button"
                       :on-click #(open-sheet-popout! data (or linked-hash ""))}
                      "Pop out")
                    ($ :button.button.button-danger
                      {:type "button"
                       :on-click
                       #(do
                          (when (= editing-id id) (set-editing-id nil))
                          (dispatch :character-sheets/remove id))}
                      "Remove")))
                (when editing?
                  ($ sheet-editor
                    {:id id
                     :sheet (:character-sheet/data entry)
                     :on-cancel #(set-editing-id nil)})))))))
      ($ :fieldset.fieldset
        ($ :legend "Token Links and Defaults")
        ($ :div.form-notice
          ($ :p {:style {:margin-bottom 8}}
            "Link sheets to token images and choose the defaults used for newly placed tokens.")
          (if (empty? tokens)
            ($ :p "No token images yet. Upload token images in the Tokens tab first.")
            ($ :ul.character-token-list
              (for [token tokens
                    :let [sheet (:token-image/character-sheet token)
                          selected-id (some (fn [{:character-sheet/keys [id data]}]
                                              (when (= data sheet) (str id))) sheets)]]
                ($ :li.character-token-list-item
                  {:key (:image/hash token)}
                  ($ :.character-token-heading
                    ($ :strong (label-for-token token))
                    ($ :p {:style {:margin 0 :opacity 0.8}}
                      (if sheet
                        (str "Linked: " (label-for-sheet sheet))
                        "No linked character sheet")))
                  ($ :.character-token-controls
                    ($ :label.character-editor-field.character-editor-field-wide
                      ($ :span "Character sheet")
                      ($ :select.text
                        {:value (or selected-id "")
                         :disabled (empty? sheets)
                         :on-change
                         (fn [event]
                           (let [id (.. event -target -value)
                                 selected
                                 (first
                                  (filter #(= id (str (:character-sheet/id %))) sheets))]
                             (dispatch :token-images/change-character-sheet
                                       (:image/hash token)
                                       (:character-sheet/data selected))))}
                        ($ :option {:value ""} "No sheet")
                        (for [{:character-sheet/keys [id name data source]} sheets]
                          ($ :option {:key (str id) :value (str id)}
                            (str name
                                 (when (:cr data) (str " (CR " (:cr data) ")"))
                                 (when source (str " - " source)))))))
                    ($ :label.character-editor-field
                      ($ :span "Default label")
                      ($ :input.text
                        {:type "text"
                         :default-value (:token-image/default-label token)
                         :on-blur
                         #(dispatch :token-images/change-default-label
                                    (:image/hash token) (.. % -target -value))}))
                    ($ :label.character-editor-field
                      ($ :span "Default size")
                      ($ :input.text
                        {:type "number" :min 1 :step 1
                         :default-value (:token-image/default-size token)
                         :placeholder "5"
                         :on-blur
                         #(let [value (.. % -target -value)]
                            (dispatch :token-images/change-default-size
                                      (:image/hash token)
                                      (when-not (str/blank? value)
                                        (js/Number value))))}))
                    ($ :label.character-editor-field
                      ($ :span "Default light")
                      ($ :input.text
                        {:type "number" :min 0 :step 1
                         :default-value (:token-image/default-light token)
                         :placeholder "15"
                         :on-blur
                         #(let [value (.. % -target -value)]
                            (dispatch :token-images/change-default-light
                                      (:image/hash token)
                                      (when-not (str/blank? value)
                                        (js/Number value))))}))
                    ($ :label.character-token-public
                      ($ :input
                        {:type "checkbox"
                         :checked (true? (:image/public token))
                         :on-change
                         #(dispatch :token-images/change-scope
                                    (:image/hash token)
                                    (.. % -target -checked))})
                      "Public"))
                  ($ :button.button.button-neutral
                    {:type "button"
                     :disabled (nil? sheet)
                     :on-click #(open-sheet-popout! sheet (:image/hash token))}
                    "Open linked sheet"))))))))))
