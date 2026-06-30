(ns ogres.app.component.panel-data
  (:require [ogres.app.const :refer [VERSION]]
            [ogres.app.hooks :as hooks]
            [ogres.app.provider.release :as release]
            [uix.core :as uix :refer [defui $]]))

(def ^:private confirm-upgrade
  "Upgrading will delete all your local data and restore this application to its original state.")

(def ^:private confirm-delete
  "Delete all your local data and restore this application to its original state?")

(def ^:private confirm-backup
  "Backup your local data and images?")

(def ^:private confirm-restore
  "Delete all your local data and restore this application using the provided backup?")

(def ^:private query-sheets
  [{:root/character-sheets
    [:character-sheet/id
     :character-sheet/name
     :character-sheet/source
     :character-sheet/data]}])

(defui ^:memo panel []
  (let [[file-name set-file-name] (uix/use-state nil)
        [import-error set-import-error] (uix/use-state nil)
        releases (uix/use-context release/context)
        dispatch (hooks/use-dispatch)
        import! (hooks/use-document-importer)
        result (hooks/use-query query-sheets [:db/ident :root])
        sheets (:root/character-sheets result)
        input (uix/use-ref)
        import-input (uix/use-ref)]

    (hooks/use-subscribe :import/error
      (uix/use-callback
       (fn [message filename]
         (set-import-error (str "Failed to import " filename ": " message))) []))

    (hooks/use-subscribe :import/success
      (uix/use-callback
       (fn [_count]
         (set-import-error nil)) []))

    ($ :.form-help
      ($ :header ($ :h2 "Data"))
      ($ :fieldset.fieldset
        ($ :legend "Version" " [ " VERSION " ]")
        ($ :div.form-notice
          (if-let [latest (last releases)]
            (if (not= VERSION latest)
              ($ :<>
                ($ :p ($ :strong "There are updates available!"))
                ($ :p "Upgrading to the latest version will "
                  ($ :strong "delete all your local data") ". "
                  "Only upgrade if you are ready to start over from scratch.")
                ($ :br)
                ($ :button.button.button-primary
                  {:on-click
                   (fn []
                     (if-let [_ (js/confirm confirm-upgrade)]
                       (dispatch :store/reset)))} "Upgrade to latest version [ " latest " ]"))
              ($ :<>
                ($ :p ($ :strong "You're on the latest version."))
                ($ :p "Pressing this button will delete all your local data and
                       restore the application to its original state.")
                ($ :br)
                ($ :button.button.button-neutral
                  {:on-click
                   (fn []
                     (if-let [_ (js/confirm confirm-delete)]
                       (dispatch :store/reset)))} "Delete local data"))))))
      ($ :fieldset.fieldset
        ($ :legend "Character Sheets")
        ($ :div.form-notice
          ($ :p {:style {:margin-bottom 4}}
            "Import character sheets from Markdown, JSON, or PDF files. Parsed
             sheets can be linked to token images in the Tokens panel.")
          ($ :button.button.button-neutral
            {:on-click #(.. import-input -current (click))}
            "Import character sheets")
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
            ($ :p {:style {:color "var(--color-red-500)" :margin-top 8}} import-error))
          (when (seq sheets)
            ($ :ul {:style {:margin-top 12 :padding-left 0 :list-style "none"}}
              (for [{:character-sheet/keys [id name source data]} sheets]
                ($ :li
                  {:key id
                   :style {:display "flex"
                           :justify-content "space-between"
                           :align-items "center"
                           :padding "4px 0"
                           :border-bottom "1px solid var(--color-neutral-200)"}}
                  ($ :span
                    (str name
                         (when (:cr data) (str " (CR " (:cr data) ")"))
                         (when source (str " — " source))))
                  ($ :button.button.button-danger
                    {:type "button"
                     :on-click #(dispatch :character-sheets/remove id)}
                    "Remove")))))))
      ($ :fieldset.fieldset
        ($ :legend "Backup and Restore")
        ($ :div.form-notice
          ($ :<>
            ($ :p {:style {:margin-bottom 4}}
              "Create a backup file that contains all your data and images. You
               can then use this file to restore your work on another computer
               or browser.")
            ($ :button.button.button-neutral
              {:on-click
               (fn []
                 (if-let [_ (js/confirm confirm-backup)]
                   (dispatch :store/create-backup)))} "Create Backup")
            ($ :br)
            ($ :p {:style {:margin-bottom 4}}
              "Select a backup file to restore your data and images. Note that "
              ($ :strong "restoring from a file will delete all your current data") ".")
            ($ :form
              {:class "form-restore"
               :on-submit
               (fn [event]
                 (.preventDefault event)
                 (let [file (first (.. input -current -files))]
                   (if (and (some? file) (js/confirm confirm-restore))
                     (dispatch :store/restore-backup file))))}
              ($ :div
                {:style
                 {:display "flex"
                  :flex-flow "row rap"
                  :gap "4px"}}
                ($ :label
                  {:for "restore-upload"
                   :class "button button-neutral"
                   :style {:box-sizing "border-box"}}
                  (or file-name "Choose file"))
                ($ :input
                  {:type "file"
                   :name "restore-upload"
                   :id "restore-upload"
                   :style {:display "none"}
                   :accept ".backup"
                   :ref input
                   :on-change
                   (fn [event]
                     (let [files (.. event -target -files)
                           file (first files)]
                       (set-file-name (.-name file))))})
                ($ :button.button.button-primary
                  {:type "submit" :disabled (if (nil? file-name) true)}
                  "Restore")))))))))
