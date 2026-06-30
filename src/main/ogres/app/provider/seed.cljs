(ns ogres.app.provider.seed
  (:require [datascript.core :as ds]
            [ogres.app.const :refer [PATH]]
            [ogres.app.import.parser :as parser]
            [ogres.app.provider.dispatch :refer [use-dispatch]]
            [ogres.app.provider.idb :as idb]
            [ogres.app.provider.image :as image]
            [ogres.app.provider.state :as state]
            [uix.core :as uix :refer [defui $]]))

(def ^:private seed-token-name "Abominable Yeti")

(defn ^:private fixture-url [filename]
  (str (if (= PATH "/release") "/dev" PATH) "/" filename))

(defn ^:private seed-empty-state! [dispatch write]
  (-> (js/fetch (fixture-url "bestiary.md"))
      (.then #(.text %))
      (.then (fn [text]
               (let [{:keys [sheets]} (parser/parse-markdown text)]
                 (dispatch :character-sheets/import sheets "bestiary.md")
                 sheets)))
      (.then (fn [sheets]
               (-> (js/fetch (fixture-url "mt28.png"))
                   (.then #(.blob %))
                   (.then (fn [blob]
                            (let [file (js/File. #js [blob] "MT28.png" #js {:type "image/png"})]
                              (image/import-token-file! file write dispatch))))
                   (.then (fn [hash]
                            (when-let [sheet (first (filter #(= seed-token-name (:name %)) sheets))]
                              (dispatch :token-images/change-character-sheet hash sheet)
                              (dispatch :token-images/change-default-label hash seed-token-name))
                            hash)))))))

(defui provider [{:keys [children]}]
  (let [conn     (uix/use-context state/context)
        dispatch (use-dispatch)
        write    (idb/use-writer "images")
        ready    (:user/ready (state/use-query [:user/ready]))
        seeded?  (uix/use-ref false)]
    (uix/use-effect
     (fn []
       (when (and ready (not @seeded?))
         (let [root   (ds/entity @conn [:db/ident :root])
               sheets (:root/character-sheets root)
               tokens (:root/token-images root)]
           (when (and (empty? sheets) (empty? tokens))
             (reset! seeded? true)
             (.catch (seed-empty-state! dispatch write) #(.error js/console %))))))
     [ready conn dispatch write])
    ($ :<> children)))
