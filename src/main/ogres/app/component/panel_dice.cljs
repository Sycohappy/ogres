(ns ogres.app.component.panel-dice
  (:require [clojure.string :as str]
            [ogres.app.component :refer [icon]]
            [ogres.app.hooks :as hooks]
            [uix.core :as uix :refer [defui $]]))

(def ^:private dice-sides [4 6 8 10 12 20 100])

(def ^:private history-limit 12)

(def ^:private query
  [{:root/user [[:session/status :default :initial]]}])

(defn ^:private parse-formula
  "Parses dice formulas such as d20, 2d6+3, or 4d8-1."
  [text]
  (when-let [s (some-> text str/trim str/lower-case not-empty)]
    (when-let [[_ n-str sides-str mod]
               (re-matches #"(\d*)d(\d+)(?:\s*([+-]\s*\d+))?" s)]
      (let [n (if (str/blank? n-str) 1 (js/parseInt n-str 10))
            sides (js/parseInt sides-str 10)
            modifier (if (str/blank? mod)
                       0
                       (js/parseInt (str/replace mod #"\s+" "") 10))]
        (when (and (<= 1 n 100) (<= 2 sides 1000))
          {:count n :sides sides :modifier modifier})))))

(defn ^:private format-formula
  [{:keys [count sides modifier]}]
  (str count "d" sides
       (cond
         (pos? modifier) (str "+" modifier)
         (neg? modifier) (str modifier)
         :else "")))

(defn ^:private roll-dice
  [{:keys [count sides modifier] :as expr}]
  (let [rolls (vec (repeatedly count #(inc (rand-int sides))))
        total (+ (reduce + rolls) modifier)]
    {:formula (format-formula expr)
     :count count
     :sides sides
     :modifier modifier
     :rolls rolls
     :total total
     :time (js/Date.now)}))

(defn ^:private format-result
  [{:keys [formula rolls modifier total]}]
  (let [detail (cond
                 (> (count rolls) 1)
                 (str " [" (str/join "+" rolls) "]"
                      (cond
                        (pos? modifier) (str "+" modifier)
                        (neg? modifier) (str modifier)
                        :else ""))

                 (not (zero? modifier))
                 (str " [" (first rolls)
                      (if (pos? modifier) "+" "")
                      modifier "]")

                 :else
                 "")]
    (str formula ": " total detail)))

(defn ^:private update-count [sides modifier set-dice-count set-formula]
  (fn [event]
    (let [value (js/parseInt (.. event -target -value) 10)]
      (when (js/isFinite value)
        (let [n (max 1 (min 100 value))]
          (set-dice-count n)
          (set-formula (format-formula {:count n :sides sides :modifier modifier})))))))

(defn ^:private update-modifier [dice-count sides set-modifier set-formula]
  (fn [event]
    (let [value (js/parseInt (.. event -target -value) 10)]
      (when (js/isFinite value)
        (let [m (max -999 (min 999 value))]
          (set-modifier m)
          (set-formula (format-formula {:count dice-count :sides sides :modifier m})))))))

(defui ^:private result-card
  [{:keys [result]}]
  (when result
    ($ :.dice-result
      {:aria-live "polite"}
      ($ :.dice-result-formula (:formula result))
      ($ :.dice-result-total (:total result))
      ($ :.dice-result-detail
        (str (str/join " + " (:rolls result))
             (cond
               (pos? (:modifier result)) (str " + " (:modifier result))
               (neg? (:modifier result)) (str " − " (- (:modifier result)))
               :else ""))))))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        status (:session/status (:root/user result))
        connected? (= status :connected)
        [dice-count set-dice-count] (uix/use-state 1)
        [sides set-sides] (uix/use-state 20)
        [modifier set-modifier] (uix/use-state 0)
        [formula set-formula] (uix/use-state "1d20")
        [last-roll set-last-roll] (uix/use-state nil)
        [history set-history] (uix/use-state [])
        [share? set-share] (uix/use-state true)
        apply-roll!
        (uix/use-callback
         (fn [expr]
           (when-let [rolled (roll-dice expr)]
             (set-last-roll rolled)
             (set-history
              (fn [prev]
                (vec (take history-limit (cons rolled prev)))))
             (set-formula (format-formula expr))
             (set-dice-count (:count expr))
             (set-sides (:sides expr))
             (set-modifier (:modifier expr))
             (when (and connected? share?)
               (dispatch :chat/send
                         (random-uuid)
                         (format-result rolled)
                         nil
                         (js/Date.now)))))
         [connected? share? dispatch])]
    ($ :.dice
      ($ :header ($ :h2 "Dice"))
      ($ result-card {:result last-roll})
      ($ :.dice-section
        ($ :h3 "Quick roll")
        ($ :.dice-quick
          (for [n dice-sides]
            ($ :button.button.button-neutral.dice-quick-btn
              {:key n
               :type "button"
               :data-selected (= sides n)
               :aria-pressed (= sides n)
               :on-click
               (fn []
                 (set-sides n)
                 (apply-roll! {:count dice-count :sides n :modifier modifier}))}
              (str "d" n)))))
      ($ :.dice-section
        ($ :h3 "Options")
        ($ :.dice-options
          ($ :label.dice-field
            ($ :span "Count")
            ($ :input.text
              {:type "number"
               :min 1
               :max 100
               :value dice-count
               :aria-label "Number of dice"
               :on-change (update-count sides modifier set-dice-count set-formula)}))
          ($ :label.dice-field
            ($ :span "Modifier")
            ($ :input.text
              {:type "number"
               :min -999
               :max 999
               :value modifier
               :aria-label "Modifier"
               :on-change (update-modifier dice-count sides set-modifier set-formula)})))
        ($ :label.checkbox.dice-share
          ($ :input
            {:type "checkbox"
             :checked share?
             :disabled (not connected?)
             :on-change #(set-share (.. % -target -checked))})
          ($ icon {:name "check" :size 16})
          (if connected?
            "Share rolls in chat"
            "Connect to share rolls in chat")))
      ($ :.dice-section
        ($ :h3 "Formula")
        ($ :form.dice-formula
          {:on-submit
           (fn [event]
             (.preventDefault event)
             (if-let [expr (parse-formula formula)]
               (apply-roll! expr)
               (set-formula (format-formula {:count dice-count :sides sides :modifier modifier}))))}
          ($ :input.text
            {:type "text"
             :value formula
             :spell-check false
             :auto-complete "off"
             :placeholder "2d6+3"
             :aria-label "Dice formula"
             :on-change #(set-formula (.. % -target -value))})
          ($ :button.button.button-primary
            {:type "submit"}
            ($ icon {:name "dice-5-fill" :size 16})
            "Roll")))
      ($ :.dice-section
        ($ :h3 "History")
        (if (seq history)
          ($ :ul.dice-history
            (for [entry history]
              ($ :li.dice-history-item
                {:key (str (:time entry) "-" (:formula entry) "-" (:total entry))}
                ($ :button.dice-history-btn
                  {:type "button"
                   :title "Roll again"
                   :on-click
                   #(apply-roll!
                     (select-keys entry [:count :sides :modifier]))}
                  ($ :span.dice-history-formula (:formula entry))
                  ($ :span.dice-history-total (:total entry))))))
          ($ :p.dice-empty "No rolls yet."))))))
