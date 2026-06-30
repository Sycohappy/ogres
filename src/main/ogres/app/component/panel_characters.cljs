(ns ogres.app.component.panel-characters
  (:require [clojure.string :as str]
            [datascript.core :as ds]
            [ogres.app.hooks :as hooks]
            [ogres.app.initiative :as initiative]
            [ogres.app.provider.state :as state]
            [uix.core :as uix :refer [defui $]]))

(def ^:private query
  [{:root/token-images
    [:image/hash
     :image/name
     :token-image/default-label
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

(defn ^:private render-action-section [entries]
  (when (seq entries)
    (str "<section class=\"section\"><h2>Actions</h2>"
         (apply str (map-indexed render-action-button entries))
         "</section>")))

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
                           "</button>")))]
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
         (or (render-entries "Traits" (:trait sheet)) "")
         (or (render-action-section (:action sheet)) "")
         (or (render-entries "Legendary Actions" (:legendary sheet)) "")
         "<details><summary>Raw JSON</summary><pre>"
         (escape-html (js/JSON.stringify (clj->js sheet) nil 2))
         "</pre></details>"
         (or (render-actions-handler (:action sheet) popup-id) "")
         (render-ability-roll-handler popup-id)
         "</body></html>")))

(defn ^:private open-sheet-popout! [sheet image-hash]
  (let [popup-id (str (random-uuid))
        actions (:action sheet)
        body (render-sheet-html sheet image-hash popup-id)
        popup (.open js/window "" "_blank" "popup,width=720,height=900")]
    (log-chat-action! "open popout"
                      {:popupId popup-id
                       :sheetName (:name sheet)
                       :imageHash image-hash
                       :actionCount (count actions)
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
        tokens   (:root/token-images result)]
    ($ :.form-help
      ($ :header ($ :h2 "Characters"))
      ($ :fieldset.fieldset
        ($ :legend "Token + Sheet Links")
        ($ :div.form-notice
          ($ :p {:style {:margin-bottom 8}}
            "Assign an imported character sheet to each token template. "
            "You can open the linked sheet in a popout window for reference.")
          (if (empty? tokens)
            ($ :p "No token images yet. Upload token images in the Tokens tab first.")
            ($ :ul {:style {:margin-top 8 :padding-left 0 :list-style "none"}}
              (for [token tokens
                    :let [sheet (:token-image/character-sheet token)
                          selected-id (some (fn [{:character-sheet/keys [id data]}]
                                              (when (= data sheet) (str id))) sheets)]]
                ($ :li
                  {:key (:image/hash token)
                   :style {:display "grid"
                           :grid-template-columns "1fr auto auto"
                           :gap 8
                           :align-items "center"
                           :padding "6px 0"
                           :border-bottom "1px solid var(--color-neutral-200)"}}
                  ($ :div
                    ($ :strong (label-for-token token))
                    ($ :p {:style {:margin 0 :opacity 0.8}}
                      (if sheet
                        (str "Linked: " (label-for-sheet sheet))
                        "No linked character sheet")))
                  ($ :select.text.text-ghost
                    {:value (or selected-id "")
                     :disabled (empty? sheets)
                     :on-change
                     (fn [event]
                       (let [id (.. event -target -value)
                             selected (first (filter #(= id (str (:character-sheet/id %))) sheets))]
                         (dispatch :token-images/change-character-sheet
                                   (:image/hash token)
                                   (:character-sheet/data selected))))}
                    ($ :option {:value ""} "No sheet")
                    (for [{:character-sheet/keys [id name data source]} sheets]
                      ($ :option {:key (str id) :value (str id)}
                        (str name
                             (when (:cr data) (str " (CR " (:cr data) ")"))
                             (when source (str " - " source))))))
                  ($ :button.button.button-neutral
                    {:type "button"
                     :disabled (nil? sheet)
                     :on-click #(open-sheet-popout! sheet (:image/hash token))}
                    "Open popout"))))))))))
