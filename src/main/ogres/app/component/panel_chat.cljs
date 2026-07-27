(ns ogres.app.component.panel-chat
  (:require [clojure.string :as str]
            [ogres.app.hooks :as hooks]
            [ogres.app.initiative :as initiative]
            [uix.core :as uix :refer [defui $]]))

(def ^:private query
  [{:root/user
    [:user/uuid
     [:user/host :default true]
     [:user/label :default ""]
     [:user/color :default "red"]
     [:session/status :default :initial]]}
   {:root/session
    [{:session/messages
      [:chat/id :chat/body :chat/src :chat/dst :chat/time]}
     {:session/host
      [:user/uuid :user/label :user/color [:user/host :default true]]}
     {:session/conns
      [:user/uuid :user/label :user/color [:user/host :default false]]}]}])

(defn ^:private user-label [user]
  (cond
    (and (some? user) (not (str/blank? (:user/label user))))
    (:user/label user)

    (:user/host user)
    "Host"

    :else
    (str/capitalize (or (:user/color user) "player"))))

(defn ^:private users-by-uuid [host conns]
  (into {}
        (for [user (cons host conns)
              :when (some? user)
              :let [uuid (:user/uuid user)]]
          [uuid user])))

(defn ^:private format-time [time]
  (when (number? time)
    (let [date (js/Date. time)]
      (.toLocaleTimeString date #js {:hour "numeric" :minute "2-digit"}))))

(defn ^:private sorted-messages [messages]
  (sort-by :chat/time messages))

(defui ^:private message-row
  [{:keys [message users self-uuid connected?]}]
  (let [dispatch (hooks/use-dispatch)
        {:chat/keys [body src dst time]} message
        sender (get users src)
        recipient (when dst (get users dst))
        whisper? (some? dst)
        outgoing? (= src self-uuid)
        incoming-whisper? (and whisper? (= dst self-uuid) (not outgoing?))
        damage-exprs (when connected?
                       (initiative/parse-damage-expressions body))
        multi-damage? (> (count damage-exprs) 1)]
    ($ :li.chat-message
      {:data-whisper whisper?
       :data-outgoing outgoing?}
      ($ :span.chat-message-dot {:data-color (:user/color sender "red")})
      ($ :div.chat-message-content
        ($ :div.chat-message-meta
          (when whisper?
            ($ :span.chat-message-tag
              (if incoming-whisper?
                (str "Whisper from " (user-label sender))
                (str "Whisper to " (user-label recipient)))))
          ($ :strong.chat-message-sender (user-label sender))
          ($ :span.chat-message-time (format-time time)))
        ($ :p.chat-message-body body)
        (when (seq damage-exprs)
          ($ :div.chat-message-actions
            (for [[idx expr] (map-indexed vector damage-exprs)]
              ($ :button.button.button-neutral.chat-damage-btn
                {:key idx
                 :type "button"
                 :on-click
                 (fn [_]
                   (when-let [msg (initiative/damage-chat-body body idx)]
                     (dispatch :chat/send (random-uuid) msg nil (js/Date.now))))
                 :title (str "Roll " (initiative/damage-button-label expr))}
                (if multi-damage?
                  (initiative/damage-button-label expr)
                  "Damage")))))))))

(defui ^:memo panel []
  (let [result (hooks/use-query query [:db/ident :root])
        {{self-uuid :user/uuid
          status :session/status} :root/user
         {messages :session/messages
          host :session/host
          conns :session/conns} :root/session} result
        users (users-by-uuid host conns)
        sorted (vec (sorted-messages (or messages [])))
        connected? (= status :connected)
        log-ref (uix/use-ref nil)]
    (uix/use-effect
     (fn []
       (when-let [node (deref log-ref)]
         (set! (.-scrollTop node) (.-scrollHeight node))))
     [sorted])
    ($ :.chat
      ($ :header ($ :h2 "Chat"))
      ($ :.chat-log
        {:ref log-ref}
        (if (= status :connected)
          (if (seq sorted)
            ($ :ul.chat-messages
              (for [message sorted]
                ($ message-row
                  {:key (:chat/id message)
                   :message message
                   :users users
                   :self-uuid self-uuid
                   :connected? connected?})))
            ($ :p.chat-empty "No messages yet. Say hello!"))
          ($ :p.chat-empty
            "Connect to an online session from the Lobby tab to chat."))))))

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {{self-uuid :user/uuid
          status :session/status} :root/user
         {host :session/host
          conns :session/conns} :root/session} result
        connected? (= status :connected)
        [recipient set-recipient] (uix/use-state "")
        [draft set-draft] (uix/use-state "")
        input-ref (uix/use-ref nil)
        recipients (remove #(= (:user/uuid %) self-uuid)
                           (cons host (or conns [])))
        send!
        (uix/use-callback
         (fn []
           (when connected?
             (let [body (str/trim draft)]
               (when (not (str/blank? body))
                 (let [id (random-uuid)
                       time (js/Date.now)
                       dst (when (not (str/blank? recipient)) recipient)]
                   (dispatch :chat/send id body dst time)
                   (set-draft "")
                   (when-let [node (deref input-ref)]
                     (.focus node)))))))
         [connected? draft recipient dispatch])]
    ($ :form.chat-compose
      {:on-submit
       (fn [event]
         (.preventDefault event)
         (send!))}
      ($ :select.text.text-ghost.chat-recipient
        {:value recipient
         :disabled (not connected?)
         :aria-label "Message recipient"
         :on-change #(set-recipient (.. % -target -value))}
        ($ :option {:value ""} "Everyone")
        (for [{:user/keys [uuid] :as user} recipients
              :when (some? uuid)]
          ($ :option {:key uuid :value uuid}
            (user-label user))))
      ($ :input.text.text-ghost.chat-input
        {:ref input-ref
         :type "text"
         :value draft
         :disabled (not connected?)
         :max-length 500
         :placeholder (if connected? "Type a message..." "Not connected")
         :aria-label "Chat message"
         :on-change #(set-draft (.. % -target -value))})
      ($ :button.button.button-primary.chat-send
        {:type "submit"
         :disabled (or (not connected?) (str/blank? (str/trim draft)))}
        "Send"))))
