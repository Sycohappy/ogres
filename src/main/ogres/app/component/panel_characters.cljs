(ns ogres.app.component.panel-characters
  (:require [clojure.string :as str]
            [ogres.app.catalog.core :as catalog]
            [ogres.app.character-sheet :as sheet]
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
     :token-image/character-sheet
     :token-image/character-sheet-id]}
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

(defn ^:private signed [n]
  (str (if (neg? n) "" "+") n))

(defn ^:private format-speed-v2 [speed]
  (when (map? speed)
    (->> speed
         (map (fn [[k v]]
                (cond
                  (number? v) (str (name k) " " v " ft.")
                  (map? v) (str (name k) " " (or (:number v) v)
                                (when (:condition v) (str " (" (:condition v) ")")))
                  :else (str (name k) " " v))))
         (str/join ", "))))

(defn ^:private render-resource-widget [res spent]
  (let [id (escape-js-string (str (:id res)))
        max-n (or (:max res) 0)
        remaining (max 0 (- max-n spent))
        kind (or (:kind res) "uses")]
    (if (= kind "pool")
      (str "<div class=\"resource-pool\" data-id=\"" id "\" data-max=\"" max-n "\" data-kind=\"pool\">"
           "<span class=\"resource-name\">" (escape-html (:name res)) "</span>"
           "<button type=\"button\" onclick=\"ogresSpendResource('" id "',1)\">−</button>"
           "<span class=\"resource-value\">" remaining " / " max-n "</span>"
           "<button type=\"button\" onclick=\"ogresSpendResource('" id "',-1)\">+</button>"
           "</div>")
      (str "<div class=\"resource-uses\" data-id=\"" id "\" data-max=\"" max-n "\" data-kind=\"uses\">"
           "<span class=\"resource-name\">" (escape-html (:name res)) "</span>"
           (apply str
                  (for [i (range max-n)]
                    (str "<button type=\"button\" class=\"use-box"
                         (when (< i spent) " used")
                         "\" data-index=\"" i "\" onclick=\"ogresSpendResource('" id "',"
                         (if (< i spent) "-1" "1")
                         ")\"></button>")))
           "</div>"))))

(defn ^:private render-slot-widget [level max-n spent]
  (let [lvl (escape-js-string (str level))
        remaining (max 0 (- max-n spent))
        label (if (= (str level) "0") "Cantrips" (str "Level " level))]
    (str "<div class=\"slot-uses resource-uses\" data-level=\"" lvl "\" data-max=\"" max-n "\" data-kind=\"slots\">"
         "<span class=\"resource-name\">" (escape-html label)
         " <span class=\"muted\">(" remaining "/" max-n ")</span></span>"
         (apply str
                (for [i (range max-n)]
                  (str "<button type=\"button\" class=\"use-box"
                       (when (< i spent) " used")
                       "\" data-index=\"" i "\" onclick=\"ogresExpendSlot('" lvl "',"
                       (if (< i spent) "-1" "1")
                       ")\"></button>")))
         "</div>")))

(defn ^:private catalog-spell-by-name [spell-name]
  (let [want (str/lower-case (str spell-name))]
    (some (fn [entry]
            (when (and (= :spell (:kind entry))
                       (= want (str/lower-case (str (:name entry)))))
              (:v2 entry)))
          (catalog/catalog))))

(defn ^:private resolve-spell [sheet spell-name]
  (or (sheet/find-spell sheet spell-name)
      (catalog-spell-by-name spell-name)
      {:name spell-name :level nil :entries []}))

(defn ^:private spell-level-num [level spell-name spell]
  (or (when (number? (:level spell)) (:level spell))
      (when-let [n (js/Number.parseInt (str level) 10)]
        (when-not (js/Number.isNaN n) n))
      0))

(defn ^:private render-spell-row [spell-name spell block spell-idx]
  (let [name (or (:name spell) spell-name)
        range (or (:range spell) "—")
        hit-dc (or (sheet/format-spell-hit-dc spell block) "—")
        dmg (when (seq (:damage spell))
              (str/join " / " (map sheet/format-damage-option (:damage spell))))
        conc? (:concentration spell)
        meta (->> [(when (:time spell) (:time spell))
                   (when (:school spell) (:school spell))
                   (when (:components spell) (:components spell))]
                  (remove nil?)
                  (str/join " · "))]
    (str "<div class=\"attack-row spell-row\">"
         "<div>"
         "<button type=\"button\" class=\"name\" title=\"Cast: send to chat"
         (when (pos? (or (:level spell) 0)) " and expend a slot")
         "\" onclick=\"ogresCastSpell(" spell-idx ")\">"
         (escape-html name) "</button>"
         (when (seq meta)
           (str "<div class=\"muted\" style=\"font-size:11px;margin-top:2px\">"
                (escape-html meta) "</div>"))
         (when (seq (:entries spell))
           (str "<details class=\"spell-detail\"><summary class=\"muted\">Details</summary>"
                "<p class=\"muted\" style=\"margin:6px 0 0;white-space:pre-wrap;font-size:12px\">"
                (escape-html (str/join "\n\n" (:entries spell)))
                "</p></details>"))
         "</div>"
         "<span class=\"muted\">" (escape-html range)
         (when conc? " <span class=\"conc\" title=\"Concentration\">C</span>")
         "</span>"
         "<span class=\"hit-dc\">" (escape-html hit-dc) "</span>"
         "<span class=\"dmg\">" (escape-html (or dmg "—")) "</span>"
         "</div>")))

(defn ^:private prepared-entries [block]
  (let [prepared (or (:prepared block) {})]
    (->> prepared
         (map (fn [[lvl spells]]
                [(str (if (keyword? lvl) (name lvl) lvl))
                 (if (sequential? spells) spells [spells])]))
         (sort-by (fn [[lvl _]] (or (js/Number.parseInt lvl 10) 99))))))

(defn ^:private slots-entries [block]
  (->> (or (:slots block) {})
       (keep (fn [[k v]]
               (when (number? v)
                 [(str (if (keyword? k) (name k) k)) v])))
       (sort-by (fn [[lvl _]] (or (js/Number.parseInt lvl 10) 99)))))

(defn ^:private render-sheet-html [raw-sheet image-hash popup-id sheet-id]
  (let [s (sheet/ensure-runtime raw-sheet)
        v2? (sheet/v2? s)
        title (sheet/sheet-name s)
        ident (sheet/identity-block s)
        abilities (sheet/abilities-map s)
        skills (sheet/skills-map s)
        attacks (sheet/attacks s)
        features (sheet/features s)
        resources (sheet/resources s)
        runtime (sheet/runtime-hp s)
        max-hp (or (sheet/hp-max s) 0)
        ac (sheet/ac-value s)
        ac-from (sheet/ac-from s)
        speed (format-speed-v2 (sheet/speed-map s))
        init-bonus (sheet/initiative-bonus s)
        pb (sheet/proficiency-bonus s)
        spellcasting (sheet/spellcasting s)
        inventory (sheet/inventory s)
        effects (sheet/effects s)
        masteries (or (get-in s [:vitals :weaponMasteries])
                      (keep (fn [a] (when (:mastery a)
                                      {:name (:name a) :mastery (:mastery a)}))
                            attacks))
        clickable
        (vec
         (concat
          (map (fn [a]
                 {:name (:name a)
                  :description (sheet/format-attack-description a)
                  :bonus (:bonus a)
                  :damage (:damage a)})
               attacks)
          (map (fn [f]
                 {:name (:name f)
                  :description (str/join " " (or (:entries f) []))})
               (filter #(contains? #{:action :bonus :reaction :free}
                                   (keyword (:economy %)))
                       features))))
        castable-spells
        (vec
         (mapcat
          (fn [block]
            (mapcat
             (fn [[lvl spells]]
               (map (fn [spell-name]
                      (let [spell (resolve-spell s spell-name)
                            level (spell-level-num lvl spell-name spell)]
                        {:name (or (:name spell) spell-name)
                         :level level
                         :description (sheet/format-spell-description spell block)
                         :bonus (when (:spellAttack spell) (:attackBonus block))
                         :damage (:damage spell)}))
                    spells))
             (prepared-entries block)))
          spellcasting))
        sheet-id-js (escape-js-string (or sheet-id ""))]
    (str "<!doctype html><html><head><meta charset=\"utf-8\"/>"
         "<title>" (escape-html title) "</title>"
         "<style>"
         "*{box-sizing:border-box}"
         "body{margin:0;font-family:Segoe UI,Helvetica,Arial,sans-serif;background:#12141a;color:#e8e8e8;}"
         ".sheet{display:flex;flex-direction:column;min-height:100vh}"
         ".header{padding:16px 20px;border-bottom:1px solid #2a2e38;background:#1a1d24}"
         ".header h1{margin:0 0 4px;font-size:26px;font-weight:700;color:#fff}"
         ".subheader{color:#b8b8b8;font-size:13px}"
         ".toolbar{display:flex;gap:8px;flex-wrap:wrap;margin-top:10px}"
         ".btn{background:#2a2e38;border:1px solid #3a4050;color:#eee;border-radius:6px;padding:6px 10px;cursor:pointer;font:inherit}"
         ".btn:hover{background:#343a48}"
         ".btn-accent{background:#8b1e2d;border-color:#a5283a}"
         ".btn-accent:hover{background:#a5283a}"
         ".layout{display:grid;grid-template-columns:220px 1fr 240px;gap:12px;padding:12px;flex:1}"
         "@media(max-width:960px){.layout{grid-template-columns:1fr}}"
         ".panel{background:#1a1d24;border:1px solid #2a2e38;border-radius:10px;padding:12px}"
         ".panel h2{margin:0 0 10px;font-size:12px;letter-spacing:.08em;text-transform:uppercase;color:#d4a84b}"
         ".ability{display:grid;grid-template-columns:1fr auto auto;gap:6px;align-items:center;padding:6px 0;border-bottom:1px solid #252833}"
         ".ability button,.skill button,.attack-row button,.feature-row button{background:transparent;border:0;color:inherit;cursor:pointer;font:inherit;text-align:left;padding:0}"
         ".ability-score{font-size:18px;font-weight:700}"
         ".ability-mod,.skill-mod{color:#f0f0f0;font-weight:700}"
         ".skill{display:flex;justify-content:space-between;gap:8px;padding:4px 0;font-size:13px}"
         ".prof-dot{width:8px;height:8px;border-radius:50%;display:inline-block;margin-right:6px;background:#444}"
         ".prof-dot.on{background:#c0392b}"
         ".tabs{display:flex;gap:4px;margin-bottom:10px;flex-wrap:wrap}"
         ".tab{background:#12141a;border:1px solid #2a2e38;color:#aaa;border-radius:6px 6px 0 0;padding:6px 10px;cursor:pointer;font:inherit}"
         ".tab.active{background:#222632;color:#fff;border-bottom-color:#222632}"
         ".tab-panel{display:none}.tab-panel.active{display:block}"
         ".attack-row,.feature-row{display:grid;grid-template-columns:1.4fr .7fr .7fr 1fr;gap:8px;padding:8px 0;border-bottom:1px solid #252833;font-size:13px;align-items:start}"
         ".attack-row .name,.feature-row .name{font-weight:700;color:#fff}"
         ".attack-row:hover,.spell-row:hover{background:#1e222c}"
         ".spell-cols{display:grid;grid-template-columns:1.4fr .7fr .7fr 1fr;gap:8px;padding:4px 0;font-size:11px;color:#9aa0ad;text-transform:uppercase;letter-spacing:.04em;border-bottom:1px solid #2a2e38}"
         ".spell-detail{margin-top:4px}.spell-detail summary{cursor:pointer}"
         ".conc{display:inline-block;margin-left:4px;padding:0 4px;border:1px solid #9aa0ad;border-radius:3px;font-size:10px;font-weight:700}"
         ".hit-dc{color:#f0d78c}"
         ".muted{color:#9aa0ad}"
         ".dmg{color:#d4a84b}"
         ".hp-block{text-align:center}"
         ".hp-current{font-size:36px;font-weight:700}"
         ".hp-sub{color:#9aa0ad;font-size:12px;margin-bottom:8px}"
         ".hp-actions{display:flex;gap:6px;justify-content:center;margin-bottom:8px}"
         ".ac-speed{display:grid;grid-template-columns:1fr 1fr;gap:8px;margin:12px 0}"
         ".stat-box{background:#12141a;border-radius:8px;padding:10px;text-align:center}"
         ".stat-box .v{font-size:24px;font-weight:700}"
         ".stat-box .l{font-size:11px;color:#9aa0ad;text-transform:uppercase}"
         ".resource-pool,.resource-uses{display:flex;align-items:center;gap:6px;flex-wrap:wrap;margin:6px 0;font-size:12px}"
         ".resource-pool button,.use-box{background:#2a2e38;border:1px solid #3a4050;color:#eee;border-radius:4px;min-width:22px;height:22px;cursor:pointer}"
         ".use-box.used{background:#c0392b;border-color:#c0392b}"
         ".resource-value{font-weight:700}"
         ".section-label{margin:14px 0 6px;font-size:12px;color:#d4a84b;text-transform:uppercase;letter-spacing:.06em}"
         ".effect{background:#222632;border-radius:6px;padding:6px 8px;margin:4px 0;font-size:12px}"
         "</style></head><body>"
         "<div class=\"sheet\" data-sheet-id=\"" sheet-id-js "\">"
         "<div class=\"header\">"
         "<h1>" (escape-html title) "</h1>"
         "<div class=\"subheader\">"
         (escape-html
          (str/join " · "
                    (remove str/blank?
                            [(when (:class ident)
                               (str (:class ident)
                                    (when (:level ident) (str " " (:level ident)))))
                             (:species ident)
                             (:background ident)
                             (when-let [xp (:xp ident)]
                               (str "XP " (:current xp) "/" (:next xp)))
                             (str "PB +" pb)])))
         "</div>"
         "<div class=\"toolbar\">"
         "<button class=\"btn btn-accent\" type=\"button\" onclick=\"ogresInitiativeRoll()\">Initiative "
         (escape-html (signed init-bonus)) "</button>"
         "<button class=\"btn\" type=\"button\" onclick=\"ogresRest('short-rest')\">Short Rest</button>"
         "<button class=\"btn\" type=\"button\" onclick=\"ogresRest('long-rest')\">Long Rest</button>"
         "<span class=\"initiative-result muted\"></span>"
         "</div></div>"
         "<div class=\"layout\">"
         ;; LEFT
         "<aside class=\"panel\">"
         "<h2>Abilities</h2>"
         (apply str
                (for [[kw {:keys [score modifier save proficient]}] abilities]
                  (let [label (str/upper-case (name kw))]
                    (str "<div class=\"ability\">"
                         "<button type=\"button\" onclick=\"ogresAbilityRoll('" label "'," score ")\">"
                         label "</button>"
                         "<span class=\"ability-score\">" score "</span>"
                         "<button type=\"button\" class=\"ability-mod\" title=\"Save "
                         (signed save) (when proficient " (proficient)") "\" "
                         "onclick=\"ogresSaveRoll('" label "'," save ")\">"
                         (signed modifier) "</button></div>"))))
         "<h2 style=\"margin-top:16px\">Skills</h2>"
         (apply str
                (for [[kw {:keys [modifier proficient]}] (sort-by (comp name first) skills)]
                  (str "<div class=\"skill\">"
                       "<button type=\"button\" onclick=\"ogresSkillRoll('"
                       (escape-js-string (str/capitalize (name kw))) "'," modifier ")\">"
                       "<span class=\"prof-dot" (when proficient " on") "\"></span>"
                       (escape-html (str/capitalize (name kw)))
                       "</button>"
                       "<span class=\"skill-mod\">" (signed modifier) "</span></div>")))
         "</aside>"
         ;; CENTER
         "<main class=\"panel\">"
         "<div class=\"tabs\">"
         "<button class=\"tab active\" type=\"button\" data-tab=\"combat\">Combat</button>"
         "<button class=\"tab\" type=\"button\" data-tab=\"spells\">Spells</button>"
         "<button class=\"tab\" type=\"button\" data-tab=\"inventory\">Inventory</button>"
         "<button class=\"tab\" type=\"button\" data-tab=\"features\">Features</button>"
         "<button class=\"tab\" type=\"button\" data-tab=\"notes\">Notes</button>"
         "</div>"
         "<div class=\"tab-panel active\" data-panel=\"combat\">"
         "<div class=\"section-label\">Attacks</div>"
         (if (seq attacks)
           (apply str
                  (map-indexed
                   (fn [idx a]
                     (str "<div class=\"attack-row\">"
                          "<button type=\"button\" class=\"name\" onclick=\"ogresPostAction(" idx ")\">"
                          (escape-html (:name a)) "</button>"
                          "<span class=\"muted\">" (escape-html (or (:range a) "—")) "</span>"
                          "<span>" (if (:bonus a) (str "+" (:bonus a) " Attack")
                                       (when-let [sv (:save a)]
                                         (str (str/upper-case (name (:ability sv))) " " (:dc sv))))
                          "</span>"
                          "<span class=\"dmg\">"
                          (escape-html
                           (str/join " / "
                                     (map sheet/format-damage-option (or (:damage a) []))))
                          "</span></div>"))
                   attacks))
           "<p class=\"muted\">No structured attacks.</p>")
         (when (seq masteries)
           (str "<div class=\"section-label\">Weapon Mastery</div>"
                (apply str
                       (for [m masteries]
                         (str "<div class=\"muted\">" (escape-html (:name m))
                              (when (:mastery m)
                                (str " (" (escape-html (:mastery m)) ")"))
                              "</div>")))))
         (apply str
                (for [economy [:action :bonus :reaction :free]
                      :let [items (filterv #(= economy (keyword (:economy %))) features)]
                      :when (seq items)]
                  (str "<div class=\"section-label\">" (escape-html (str/upper-case (name economy))) "S</div>"
                       (apply str
                              (for [f items
                                    :let [idx (some (fn [[i a]]
                                                      (when (= (:name a) (:name f)) i))
                                                    (map-indexed vector clickable))]]
                                (str "<div class=\"feature-row\" style=\"grid-template-columns:1fr 2fr\">"
                                     "<button type=\"button\" class=\"name\" onclick=\"ogresPostAction("
                                     (or idx 0) ")\">"
                                     (escape-html (:name f)) "</button>"
                                     "<span class=\"muted\">"
                                     (escape-html (str/join " " (or (:entries f) [])))
                                     "</span></div>"
                                     (when-let [rid (:resourceId f)]
                                       (when-let [res (sheet/resource-by-id s rid)]
                                         (render-resource-widget
                                          res (sheet/resource-spent s rid))))))))))
         "</div>"
         "<div class=\"tab-panel\" data-panel=\"spells\">"
         (if (seq spellcasting)
           (let [spell-idx (atom 0)]
             (apply str
                    (for [block spellcasting]
                      (str "<div class=\"section-label\">" (escape-html (or (:name block) "Spellcasting")) "</div>"
                           (when (:dc block)
                             (str "<p class=\"muted\">DC " (:dc block)
                                  (when (:attackBonus block) (str " · Attack +" (:attackBonus block)))
                                  "</p>"))
                           (let [slot-rows (slots-entries block)]
                             (when (seq slot-rows)
                               (str "<div class=\"slot-trackers\" style=\"margin:8px 0 12px\">"
                                    (apply str
                                           (for [[lvl max-n] slot-rows]
                                             (render-slot-widget
                                              lvl max-n (sheet/slots-expended s lvl))))
                                    "</div>")))
                           (when (seq (prepared-entries block))
                             (str "<div class=\"spell-cols\"><span>Name</span><span>Range</span><span>Hit / DC</span><span>Damage</span></div>"))
                           (apply str
                                  (for [[lvl spells] (prepared-entries block)]
                                    (str "<div class=\"section-label\" style=\"margin-top:12px\">"
                                         (if (= lvl "0") "Cantrips" (str "Level " (escape-html lvl)))
                                         "</div>"
                                         (apply str
                                                (for [spell-name spells
                                                      :let [i @spell-idx
                                                            _ (swap! spell-idx inc)
                                                            spell (resolve-spell s spell-name)]]
                                                  (render-spell-row spell-name spell block i))))))))))
           "<p class=\"muted\">No spellcasting data.</p>")
         "</div>"
         "<div class=\"tab-panel\" data-panel=\"inventory\">"
         (let [items (or (:items inventory) [])]
           (if (seq items)
             (apply str
                    (for [item items]
                      (str "<div class=\"feature-row\" style=\"grid-template-columns:1fr auto\">"
                           "<span class=\"name\">" (escape-html (:name item)) "</span>"
                           "<span class=\"muted\">" (when (:attuned item) "Attuned") "</span></div>")))
             "<p class=\"muted\">Inventory empty.</p>"))
         "</div>"
         "<div class=\"tab-panel\" data-panel=\"features\">"
         (apply str
                (for [f (filter #(= :trait (keyword (:economy %))) features)]
                  (str "<p><strong>" (escape-html (:name f)) ".</strong> "
                       (escape-html (str/join " " (or (:entries f) []))) "</p>")))
         "</div>"
         "<div class=\"tab-panel\" data-panel=\"notes\">"
         "<p class=\"muted\">Sheet version " (if v2? "2" "1")
         (when-let [src (:source (meta raw-sheet))] (str " · " src))
         "</p>"
         "<details><summary>Raw JSON</summary><pre style=\"white-space:pre-wrap;font-size:11px;color:#ccc\">"
         (escape-html (js/JSON.stringify (clj->js raw-sheet) nil 2))
         "</pre></details>"
         "</div>"
         "</main>"
         ;; RIGHT
         "<aside class=\"panel\">"
         "<div class=\"hp-block\">"
         "<div class=\"hp-current\" id=\"hpCurrent\">" (or (:current runtime) max-hp) "</div>"
         "<div class=\"hp-sub\" id=\"hpSub\">HP / " max-hp
         (when (pos? (or (:temp runtime) 0))
           (str " · Temp " (:temp runtime)))
         "</div>"
         "<div class=\"hp-actions\">"
         "<button class=\"btn btn-accent\" type=\"button\" onclick=\"ogresChangeHp(1)\">Damage</button>"
         "<button class=\"btn\" type=\"button\" onclick=\"ogresChangeHp(-1)\">Heal</button>"
         "</div>"
         "<input id=\"hpDelta\" type=\"number\" min=\"1\" value=\"1\" style=\"width:64px;background:#12141a;border:1px solid #2a2e38;color:#eee;border-radius:4px;padding:4px\"/>"
         "</div>"
         "<div class=\"ac-speed\">"
         "<div class=\"stat-box\"><div class=\"v\">" (or ac "—") "</div><div class=\"l\">Armor Class</div>"
         (when (seq ac-from)
           (str "<div class=\"muted\" style=\"font-size:10px;margin-top:4px\">"
                (escape-html (str/join ", " ac-from)) "</div>"))
         "</div>"
         "<div class=\"stat-box\"><div class=\"v\" style=\"font-size:16px\">"
         (escape-html (or speed "—")) "</div><div class=\"l\">Speed</div></div>"
         "</div>"
         "<h2>Defenses</h2>"
         "<p class=\"muted\">Resistances: "
         (escape-html (or (format-list (sheet/resistances s)) "—")) "</p>"
         "<h2>Senses</h2>"
         "<p class=\"muted\">" (escape-html (or (format-list (sheet/senses s)) "—")) "</p>"
         "<h2>Languages</h2>"
         "<p class=\"muted\">" (escape-html (or (format-list (sheet/languages s)) "—")) "</p>"
         (when (seq resources)
           (str "<h2>Resources</h2>"
                (apply str
                       (for [res resources]
                         (render-resource-widget res (sheet/resource-spent s (:id res)))))))
         (when (seq effects)
           (str "<h2>Effects</h2>"
                (apply str
                       (for [fx effects]
                         (str "<div class=\"effect\">"
                              (escape-html (or (:name fx) "Effect"))
                              " · " (or (:rounds-remaining fx) "?") " rnd"
                              (when (:concentration fx) " · Conc")
                              "</div>")))))
         "</aside></div></div>"
         "<script>"
         "window.ogresPopupId=\"" (escape-js-string popup-id) "\";"
         "window.ogresSheetId=\"" sheet-id-js "\";"
         "window.ogresImageHash=\"" (escape-js-string image-hash) "\";"
         "window.ogresActions=" (.stringify js/JSON (clj->js clickable)) ";"
         "window.ogresSpells=" (.stringify js/JSON (clj->js castable-spells)) ";"
         "function post(msg){if(!window.opener)return;msg.popupId=window.ogresPopupId;msg.sheetId=window.ogresSheetId;window.opener.postMessage(msg,window.location.origin);}"
         "window.ogresPostAction=function(idx){var action=window.ogresActions[idx];if(!action)return;post({type:\"ogres:chat-action\",name:action.name,description:action.description||\"\",bonus:action.bonus||null,damage:action.damage||null});};"
         "window.ogresCastSpell=function(idx){var spell=window.ogresSpells[idx];if(!spell)return;"
         "post({type:\"ogres:chat-action\",name:spell.name,description:spell.description||\"\",bonus:spell.bonus||null,damage:spell.damage||null});"
         "if(spell.level>0){post({type:\"ogres:expend-slot\",level:String(spell.level),amount:1});}"
         "};"
         "window.ogresPostNamedAction=window.ogresPostAction;"
         "window.ogresAbilityRoll=function(ability,score){post({type:\"ogres:ability-roll\",ability:ability,score:score});};"
         "window.ogresSaveRoll=function(ability,mod){post({type:\"ogres:skill-roll\",label:ability+\" Save\",modifier:mod});};"
         "window.ogresSkillRoll=function(label,mod){post({type:\"ogres:skill-roll\",label:label,modifier:mod});};"
         "window.ogresInitiativeRoll=function(){post({type:\"ogres:initiative-roll\",hash:window.ogresImageHash});};"
         "window.ogresSpendResource=function(id,amount){post({type:\"ogres:spend-resource\",resourceId:id,amount:amount});};"
         "window.ogresExpendSlot=function(level,amount){post({type:\"ogres:expend-slot\",level:level,amount:amount});};"
         "window.ogresRest=function(kind){post({type:\"ogres:rest\",kind:kind});};"
         "window.ogresChangeHp=function(sign){var n=parseInt(document.getElementById(\"hpDelta\").value,10)||1;post({type:\"ogres:change-hp\",delta:sign*n});};"
         "window.ogresApplyRuntime=function(data){"
         "if(!data)return;"
         "var hp=data.hp||{};"
         "var curEl=document.getElementById(\"hpCurrent\");"
         "var subEl=document.getElementById(\"hpSub\");"
         "if(curEl&&hp.current!=null)curEl.textContent=String(hp.current);"
         "if(subEl){var t=\"HP / \"+(hp.max!=null?hp.max:\"?\");if(hp.temp>0)t+=\" · Temp \"+hp.temp;subEl.textContent=t;}"
         "var spent=data.resourceSpent||{};"
         "document.querySelectorAll(\".resource-pool,.resource-uses:not(.slot-uses)\").forEach(function(el){"
         "var id=el.getAttribute(\"data-id\");"
         "var max=parseInt(el.getAttribute(\"data-max\"),10)||0;"
         "var s=spent[id];if(s==null)s=0;"
         "if(el.getAttribute(\"data-kind\")===\"pool\"){"
         "var val=el.querySelector(\".resource-value\");"
         "if(val)val.textContent=Math.max(0,max-s)+\" / \"+max;"
         "}else{"
         "el.querySelectorAll(\".use-box\").forEach(function(btn,i){"
         "if(i<s)btn.classList.add(\"used\");else btn.classList.remove(\"used\");"
         "btn.setAttribute(\"onclick\",\"ogresSpendResource('\"+id+\"',\"+(i<s?-1:1)+\")\");"
         "});"
         "}"
         "});"
         "var slots=data.slotsExpended||{};"
         "document.querySelectorAll(\".slot-uses\").forEach(function(el){"
         "var level=el.getAttribute(\"data-level\");"
         "var max=parseInt(el.getAttribute(\"data-max\"),10)||0;"
         "var s=slots[level];if(s==null)s=0;"
         "var name=el.querySelector(\".resource-name\");"
         "if(name){var label=level===\"0\"?\"Cantrips\":(\"Level \"+level);"
         "name.innerHTML=label+' <span class=\"muted\">('+Math.max(0,max-s)+'/'+max+')</span>';}"
         "el.querySelectorAll(\".use-box\").forEach(function(btn,i){"
         "if(i<s)btn.classList.add(\"used\");else btn.classList.remove(\"used\");"
         "btn.setAttribute(\"onclick\",\"ogresExpendSlot('\"+level+\"',\"+(i<s?-1:1)+\")\");"
         "});"
         "});"
         "};"
         "document.querySelectorAll(\".tab\").forEach(function(tab){tab.addEventListener(\"click\",function(){document.querySelectorAll(\".tab\").forEach(function(t){t.classList.remove(\"active\")});document.querySelectorAll(\".tab-panel\").forEach(function(p){p.classList.remove(\"active\")});tab.classList.add(\"active\");document.querySelector('[data-panel=\"'+tab.getAttribute(\"data-tab\")+'\"]').classList.add(\"active\");});});"
         "window.addEventListener(\"message\",function(event){"
         "if(event.origin!==window.location.origin)return;"
         "var data=event.data;"
         "if(!data||data.popupId!==window.ogresPopupId)return;"
         "if(data.type===\"ogres:initiative-roll-result\"){"
         "var el=document.querySelector(\".initiative-result\");if(!el)return;"
         "if(data.error){el.textContent=data.error;return;}"
         "var modStr=data.modifier>=0?\"+\"+data.modifier:String(data.modifier);"
         "el.textContent=data.total+\" (\"+data.die+\" \"+modStr+\")\"+(data.count>1?\" ×\"+data.count:\"\");"
         "}"
         "if(data.type===\"ogres:sheet-runtime\"){window.ogresApplyRuntime(data);}"
         "});"
         "</script>"
         "</body></html>")))

(defn ^:private open-sheet-popout! [sheet image-hash sheet-id]
  (let [popup-id (str (random-uuid))
        body (render-sheet-html sheet image-hash popup-id sheet-id)
        popup (.open js/window "" "_blank" "popup,width=1280,height=900")]
    (log-chat-action! "open popout"
                      {:popupId popup-id
                       :sheetName (sheet/sheet-name sheet)
                       :imageHash image-hash
                       :sheetId sheet-id
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

(defn ^:private resolve-library-sheet-id
  "Library sheet id for a linked token sheet map (id, equality, then name)."
  ([sheets linked-sheet]
   (resolve-library-sheet-id sheets linked-sheet nil))
  ([sheets linked-sheet prefer-id]
   (or (when (and prefer-id (some #(= (str (:character-sheet/id %)) (str prefer-id)) sheets))
         (str prefer-id))
       (some (fn [{:character-sheet/keys [id data]}]
               (when (= data linked-sheet) (str id)))
             sheets)
       (when (map? linked-sheet)
         (let [want (sheet/sheet-name linked-sheet)]
           (some (fn [{:character-sheet/keys [id data]}]
                   (when (= want (sheet/sheet-name data)) (str id)))
                 sheets)))
       "")))

(defn ^:private reply-sheet-runtime! [conn popup-id sheet-id]
  (when (and (not-empty popup-id) (not-empty sheet-id))
    (when-let [entry (ds/entity @conn [:character-sheet/id sheet-id])]
      (reply-to-popout!
       popup-id
       (merge {:type "ogres:sheet-runtime"
               :popupId popup-id
               :sheetId sheet-id}
              (sheet/runtime-snapshot (:character-sheet/data entry)))))))

(defui initiative-popout-listeners []
  (let [conn     (uix/use-context state/context)
        dispatch (hooks/use-dispatch)]
    (hooks/use-subscribe
     :character-sheets/open-popout
     (uix/use-callback
      (fn [sheet hash sheet-id]
        (when (map? sheet)
          (open-sheet-popout! sheet (or hash "") (or sheet-id ""))))
      []))
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
                           sheet (or (:token-image/character-sheet
                                      (ds/entity db [:image/hash hash]))
                                     (when-let [sid (.-sheetId data)]
                                       (:character-sheet/data
                                        (ds/entity db [:character-sheet/id sid]))))]
                       (if (and (empty? ids) (nil? sheet))
                         (reply-to-popout!
                          popup-id
                          {:type "ogres:initiative-roll-result"
                           :popupId popup-id
                           :error "No sheet or tokens available"})
                         (let [result (initiative/roll-result sheet)]
                           (when (seq ids)
                             (dispatch :initiative/roll-shared ids sheet result))
                           (reply-to-popout!
                            popup-id
                            (merge {:type "ogres:initiative-roll-result"
                                    :popupId popup-id
                                    :count (max 1 (count ids))}
                                   result)))))

                     "ogres:chat-action"
                     (let [popup-id (.-popupId data)
                           name (.-name data)
                           description (.-description data)
                           bonus (.-bonus data)
                           body (initiative/action-chat-body name description bonus)
                           user (ds/entity @conn [:db/ident :user])
                           status (:session/status user)]
                       (log-chat-action! "received"
                                         {:popupId popup-id
                                          :name name
                                          :description description
                                          :bonus bonus
                                          :body body
                                          :session/status status})
                       (if (= status :connected)
                         (dispatch :chat/send (random-uuid) body nil (js/Date.now))
                         (reply-to-popout!
                          popup-id
                          {:type "ogres:chat-action-result"
                           :popupId popup-id
                           :error "Connect to a session to chat"})))

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

                     "ogres:skill-roll"
                     (let [label (.-label data)
                           modifier (.-modifier data)
                           user (ds/entity @conn [:db/ident :user])]
                       (when (= (:session/status user) :connected)
                         (let [die (inc (rand-int 20))
                               total (+ die modifier)
                               mod-str (if (neg? modifier) (str modifier) (str "+" modifier))
                               body (str label " — " total " (d20 " mod-str ")")]
                           (dispatch :chat/send (random-uuid) body nil (js/Date.now)))))

                     "ogres:spend-resource"
                     (when-let [sid (not-empty (.-sheetId data))]
                       (let [popup-id (.-popupId data)]
                         (dispatch :character-sheets/spend-resource
                                   sid (.-resourceId data) (.-amount data))
                         (reply-sheet-runtime! conn popup-id sid)))

                     "ogres:expend-slot"
                     (when-let [sid (not-empty (.-sheetId data))]
                       (let [popup-id (.-popupId data)]
                         (dispatch :character-sheets/expend-slot
                                   sid (.-level data) (.-amount data))
                         (reply-sheet-runtime! conn popup-id sid)))

                     "ogres:rest"
                     (when-let [sid (not-empty (.-sheetId data))]
                       (let [popup-id (.-popupId data)]
                         (dispatch :character-sheets/rest sid (.-kind data))
                         (reply-sheet-runtime! conn popup-id sid)))

                     "ogres:change-hp"
                     (when-let [sid (not-empty (.-sheetId data))]
                       (let [popup-id (.-popupId data)]
                         (dispatch :character-sheets/change-hp sid (.-delta data))
                         (reply-sheet-runtime! conn popup-id sid)))

                     "ogres:set-temp-hp"
                     (when-let [sid (not-empty (.-sheetId data))]
                       (let [popup-id (.-popupId data)]
                         (dispatch :character-sheets/set-temp-hp sid (.-temp data))
                         (reply-sheet-runtime! conn popup-id sid)))

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
        catalog-input (uix/use-ref nil)
        [editing-id set-editing-id] (uix/use-state nil)
        [import-error set-import-error] (uix/use-state nil)
        [import-success set-import-success] (uix/use-state nil)
        [catalog-query set-catalog-query] (uix/use-state "")
        [catalog-msg set-catalog-msg] (uix/use-state nil)
        catalog-hits (catalog/search catalog-query)]
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
    (hooks/use-subscribe
     :import/catalog
     (uix/use-callback
      (fn [result]
        (set-import-error nil)
        (set-import-success nil)
        (set-catalog-msg
         (str "Loaded catalog from " (:source result)
              " (" (:added result) " entries"
              (when (:merged result) ", merged")
              "). Search below and Apply to an open sheet."))) []))
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
        ($ :legend "5etools Catalog (private)")
        ($ :div.form-notice
          ($ :p
            "Import privately obtained 5etools JSON (classFeature/item/spell/race). Multiple files merge in memory — do not commit WotC text to git.")
          ($ :.character-management-actions
            ($ :button.button.button-neutral
              {:type "button"
               :on-click #(.. catalog-input -current (click))}
              "Import catalog JSON")
            ($ :button.button.button-neutral
              {:type "button"
               :on-click #(do (catalog/clear!) (set-catalog-msg "Catalog cleared."))}
              "Clear catalog"))
          ($ :input
            {:type "file"
             :hidden true
             :accept ".json"
             :multiple true
             :ref catalog-input
             :on-change
             (fn [event]
               (let [files (vec (array-seq (.. event -target -files)))]
                 (when (seq files)
                   (letfn [(read-next! [idx merge? loaded]
                             (if (>= idx (count files))
                               (set-catalog-msg
                                (str "Loaded " (count (catalog/catalog))
                                     " catalog entries from "
                                     (str/join ", " loaded)))
                               (let [file (nth files idx)
                                     reader (js/FileReader.)]
                                 (set! (.-onload reader)
                                       (fn [e]
                                         (try
                                           (let [parsed (js->clj (js/JSON.parse (.. e -target -result))
                                                                 :keywordize-keys true)
                                                 _ (catalog/load-data! parsed (.-name file) merge?)]
                                             (read-next! (inc idx) true (conj loaded (.-name file))))
                                           (catch :default err
                                             (set-catalog-msg
                                              (str "Catalog import failed on " (.-name file)
                                                   ": " (.-message err)))))))
                                 (set! (.-onerror reader)
                                       (fn [_]
                                         (set-catalog-msg
                                          (str "Could not read " (.-name file)))))
                                 (.readAsText reader file))))]
                     (read-next! 0 (seq (catalog/catalog)) [])))
                 (set! (.. event -target -value) "")))})
          (when catalog-msg
            ($ :p.character-management-message catalog-msg))
          (when (seq (catalog/catalog))
            ($ :div
              ($ :p.muted
                (str (count (catalog/catalog)) " entries"
                     (when (seq (catalog/sources))
                       (str " · " (str/join ", " (catalog/sources))))))
              ($ :input.text
                {:type "text"
                 :placeholder "Search catalog (try spell names)…"
                 :value catalog-query
                 :on-change #(set-catalog-query (.. % -target -value))
                 :style {:width "100%" :margin "8px 0"}})
              ($ :ul.character-sheet-list
                (for [entry (take 20 catalog-hits)]
                  ($ :li.character-sheet-list-item
                    {:key (str (:kind entry) "-" (:name entry)
                               "-" (get-in entry [:v2 :source]))}
                    ($ :.character-sheet-summary
                      ($ :div
                        ($ :strong (:name entry))
                        ($ :p
                          (str (name (:kind entry))
                               (when (and (= (:kind entry) :spell)
                                          (number? (:level entry)))
                                 (str " · L" (:level entry))))))
                      (when editing-id
                        ($ :button.button.button-neutral
                          {:type "button"
                           :on-click
                           #(do
                              (dispatch :character-sheets/apply-catalog editing-id entry)
                              (set-catalog-msg (str "Applied " (:name entry))))}
                          "Apply to open sheet"))))))))))
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
                       :on-click #(open-sheet-popout! data (or linked-hash "") id)}
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
                          selected-id (resolve-library-sheet-id
                                       sheets sheet
                                       (:token-image/character-sheet-id token))]]
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
                     :on-click
                     #(open-sheet-popout! sheet (:image/hash token)
                                          (or selected-id ""))}
                    "Open linked sheet"))))))))))
