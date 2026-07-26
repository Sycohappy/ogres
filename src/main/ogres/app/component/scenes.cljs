(ns ogres.app.component.scenes
  (:require [clojure.string :refer [replace]]
            [ogres.app.component :refer [icon]]
            [ogres.app.hooks :as hooks]
            [uix.core :as uix :refer [defui $]]))

(def ^:private filename-re #"\d+x\d+|[^\w ]|.[^.]+$")

(defn ^:private render-scene-name [camera]
  (if-let [label (:camera/label camera)]
    label
    (if-let [filename (-> camera :camera/scene :scene/image :image/name)]
      (replace filename filename-re "")
      "Untitled scene")))

(defn ^:private render-remove-prompt [camera]
  (str "Are you sure you want to remove '" (render-scene-name camera) "'?"))

(def ^:private query
  [:user/camera
   {:user/cameras
    [:db/id
     :camera/label
     {:camera/scene
      [:db/id
       {:scene/image [:image/name]}]}]}])

(def ^:private query-session
  [{:session/active-scene [:db/id]}])

(defui scenes []
  (let [dispatch (hooks/use-dispatch)
        {current :user/camera
         cameras :user/cameras} (hooks/use-query query)
        session (hooks/use-query query-session [:db/ident :session])
        active-id (:db/id (:session/active-scene session))]
    (hooks/use-shortcut ["delete" "backspace"]
      (uix/use-callback
       (fn [event]
         (if (= (.-name (.-activeElement js/document)) "scene")
           (let [id (js/Number (.. event -originalEvent -target -value))
                 cm (first (filter (comp #{id} :db/id) cameras))]
             (if (js/confirm (render-remove-prompt cm))
               (dispatch :scenes/remove id))))) [dispatch cameras]))
    ($ :ul.scenes {:role "tablist"}
      (for [{id :db/id :as camera} cameras
            :let [selected (= id (:db/id current))
                  active (= active-id (:db/id (:camera/scene camera)))]]
        ($ :li.scenes-scene
          {:key id
           :role "tab"
           :aria-selected selected
           :data-session-active active}
          ($ :label
            ($ :input
              {:type "radio"
               :name "scene"
               :value id
               :checked selected
               :on-change (fn [event] (dispatch :scenes/change (js/Number (.. event -target -value))))})
            ($ :.scenes-label
              (render-scene-name camera))
            (if-not active
              ($ :.scenes-activate
                {:title "Set as the active scene for players."
                 :on-click
                 (fn [event]
                   (.preventDefault event)
                   (dispatch :scenes/activate id))}
                ($ icon {:name "people-fill" :size 16})))
            ($ :.scenes-remove
              {:on-click
               (fn []
                 (if (js/confirm (render-remove-prompt camera))
                   (dispatch :scenes/remove id)))}
              ($ icon {:name "x" :size 21})))))
      ($ :li.scenes-create {:role "tab"}
        ($ :button
          {:type "button"
           :title "Create a new scene."
           :on-click #(dispatch :scenes/create)}
          ($ icon {:name "plus" :size 19}))))))
