(ns events-test
  (:require [cljs.test :refer-macros [deftest is]]
            [datascript.core :as ds :refer [transact! entity]]
            [ogres.app.events :refer [event-tx-fn chat-message-tx sanitize-transaction without-chat-tx-data]]
            [ogres.app.provider.state :refer [initial-data schema]]
            [ogres.app.vec :as vec]))

(defn dispatch [conn event & args]
  (transact! conn [[:db.fn/call (fn [db] (apply event-tx-fn db event args))]]))

(defn user [conn]
  (entity @conn [:db/ident :user]))

(deftest test-chat-send-room
  (let [conn (ds/create-conn schema)
        _ (ds/reset-conn! conn (initial-data true))
        uuid (random-uuid)]
    (transact! conn [[:db/add [:db/ident :user] :user/uuid uuid]
                     [:db/add [:db/ident :user] :session/status :connected]])
    (dispatch conn :chat/send (random-uuid) "hi" nil (js/Date.now))
    (let [{msgs :session/messages} (entity @conn [:db/ident :session])
          msg (first msgs)]
      (is (= "hi" (:chat/body msg)))
      (is (not (contains? msg :chat/dst))))))

(deftest test-chat-tx-sanitize
  (let [legacy '[{:db/ident :session
                  :session/messages {:chat/id #uuid "11111111-1111-1111-1111-111111111111"
                                     :chat/body "hi"
                                     :chat/src "abc"
                                     :chat/dst nil
                                     :chat/time 123}}]]
    (is (not (contains? (:session/messages (first (sanitize-transaction legacy))) :chat/dst)))
    (is (nil? (without-chat-tx-data legacy)))))

(deftest test-panel
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :user/select-panel :session)
    (is (= (:panel/selected (user conn)) :session))

    (dispatch conn :user/toggle-panel)
    (is (= (:panel/expanded (user conn)) false))

    (dispatch conn :user/toggle-panel)
    (is (= (:panel/expanded (user conn)) true))

    (dispatch conn :user/toggle-panel)
    (dispatch conn :user/select-panel :tokens)
    (is (= (:panel/expanded (user conn)) true))
    (is (= (:panel/selected (user conn)) :tokens))))

(deftest test-scene-focus
  (let [db (initial-data true)
        sc (:db/id (:camera/scene (:user/camera (entity db [:db/ident :user]))))
        tx [{:db/ident :root
             :root/session
             {:db/ident :session
              :session/host {:db/ident :user}
              :session/conns
              [{:user/host false :user/uuid (random-uuid) :user/cameras {:db/id -1 :camera/scene sc} :user/camera -1}
               {:user/host false :user/uuid (random-uuid) :user/cameras {:db/id -2 :camera/scene sc} :user/camera -2}
               {:user/host false :user/uuid (random-uuid) :user/cameras {:db/id -3 :camera/scene sc} :user/camera -3}]}}]
        conn (ds/conn-from-db (ds/db-with db tx))]
    (dispatch conn :scenes/create)
    (dispatch conn :session/focus)
    (let [{conns :session/conns
           active :session/active-scene} (entity @conn [:db/ident :session])
          host-scene (:camera/scene (:user/camera (user conn)))]
      (is (every? (comp #{2} count :user/cameras) conns)
          "New cameras are created for users that don't already have one for the newly focused scene.")
      (is (every? #{host-scene} (map (comp :camera/scene :user/camera) conns))
          "Every user is viewing the same scene as the host.")
      (is (= active host-scene)
          "Focus updates the session's active scene to the host's scene.")
      (is (= (count (into #{} (map :user/camera) conns)) 3)
          "Every user has a distinct camera entity."))
    (dispatch conn :scenes/change sc)
    (let [{conns :session/conns} (entity @conn [:db/ident :session])]
      (is (every? (comp not #{sc} :db/id :camera/scene :user/camera) conns)
          "Users remain on the scene even if the host changes theirs."))))

(deftest test-scene-activate
  (let [db (initial-data true)
        sc (:db/id (:camera/scene (:user/camera (entity db [:db/ident :user]))))
        tx [{:db/ident :root
             :root/session
             {:db/ident :session
              :session/host {:db/ident :user}
              :session/conns
              [{:user/host false
                :user/uuid (random-uuid)
                :user/cameras {:db/id -1 :camera/scene sc :camera/point [10 20] :camera/scale 2}
                :user/camera -1}
               {:user/host false
                :user/uuid (random-uuid)
                :user/cameras {:db/id -2 :camera/scene sc :camera/point [30 40] :camera/scale 0.5}
                :user/camera -2}]}}]
        conn (ds/conn-from-db (ds/db-with db tx))]
    (dispatch conn :scenes/create)
    (let [new-camera (:user/camera (user conn))
          new-scene (:db/id (:camera/scene new-camera))
          host-camera-id (:db/id new-camera)]
      (dispatch conn :scenes/change (:db/id (first (filter (comp #{sc} :db/id :camera/scene) (:user/cameras (user conn))))))
      (dispatch conn :scenes/activate host-camera-id)
      (let [{conns :session/conns
             active :session/active-scene} (entity @conn [:db/ident :session])]
        (is (= (:db/id active) new-scene)
            "Activate sets the session's active scene.")
        (is (every? (comp #{new-scene} :db/id :camera/scene :user/camera) conns)
            "All players move to the activated scene.")
        (is (= (:db/id (:camera/scene (:user/camera (user conn)))) sc)
            "Host remains on their selected scene when activating another.")
        (is (every? (comp #{vec/zero} :camera/point :user/camera) conns)
            "New cameras for the activated scene start at the origin.")
        (is (every? (comp nil? :camera/scale :user/camera) conns)
            "Activate does not set camera scale on newly created cameras.")))
    ;; Players already have a camera for the original scene; reactivating it
    ;; must preserve their pan/zoom.
    (let [host-cam (first (filter (comp #{sc} :db/id :camera/scene) (:user/cameras (user conn))))]
      (dispatch conn :scenes/activate (:db/id host-cam))
      (let [{conns :session/conns} (entity @conn [:db/ident :session])
            points (into #{} (map (comp :camera/point :user/camera)) conns)
            scales (into #{} (map (comp :camera/scale :user/camera)) conns)]
        (is (= points #{[10 20] [30 40]})
            "Reusing an existing camera preserves pan.")
        (is (= scales #{2 0.5})
            "Reusing an existing camera preserves zoom.")
        (is (every? (comp #{sc} :db/id :camera/scene :user/camera) conns)
            "Players return to the reactivated scene.")))
    (dispatch conn :scenes/create)
    (let [created (:user/camera (user conn))]
      (dispatch conn :scenes/activate (:db/id created))
      (dispatch conn :scenes/change (:db/id (first (filter (comp #{sc} :db/id :camera/scene) (:user/cameras (user conn))))))
      (dispatch conn :scenes/remove (:db/id created))
      (let [{active :session/active-scene
             conns :session/conns} (entity @conn [:db/ident :session])]
        (is (= (:db/id active) sc)
            "Removing the active scene reassigns it to the scene the host lands on.")
        (is (every? (comp #{sc} :db/id :camera/scene :user/camera) conns)
            "Players on the removed active scene move to the new active scene.")))))
