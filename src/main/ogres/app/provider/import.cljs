(ns ogres.app.provider.import
  (:require [clojure.string :as str]
            [ogres.app.import.parser :as parser]
            [ogres.app.provider.dispatch :as dispatch]
            [ogres.app.provider.events :as events]
            [uix.core :as uix :refer [defui $]]))

(def ^:private pdfjs-url
  "https://cdn.jsdelivr.net/npm/pdfjs-dist@2.16.105/build/pdf.min.js")

(def ^:private pdfjs-worker-url
  "https://cdn.jsdelivr.net/npm/pdfjs-dist@2.16.105/build/pdf.worker.min.js")

(defonce ^:private pdfjs-loader (atom nil))

(defn ^:private debug-log
  [& parts]
  (.apply js/console.log
          (clj->js (into ["[ogres:import]"] parts))))

(defn ^:private debug-json [label value]
  (debug-log label (.stringify js/JSON (clj->js value) nil 2)))

(defn ^:private file-meta [file]
  #js {:name (.-name file)
       :size (.-size file)
       :type (.-type file)})

(defn ^:private ensure-worker-src! [pdfjs]
  (when-let [opts (aget pdfjs "GlobalWorkerOptions")]
    (aset opts "workerSrc" pdfjs-worker-url)))

(defn ^:private load-pdfjs []
  (if-let [pdfjs (aget js/window "pdfjsLib")]
    (do (debug-log "pdfjs already loaded")
        (ensure-worker-src! pdfjs)
        (js/Promise.resolve pdfjs))
    (or @pdfjs-loader
        (let [promise
              (js/Promise.
               (fn [resolve reject]
                 (debug-log "loading pdfjs" pdfjs-url)
                 (let [script (.createElement js/document "script")]
                   (set! (.-src script) pdfjs-url)
                   (set! (.-async script) true)
                   (set! (.-onload script)
                         (fn []
                           (if-let [pdfjs (aget js/window "pdfjsLib")]
                             (if-let [_ (aget pdfjs "GlobalWorkerOptions")]
                               (do (debug-log "pdfjs loaded")
                                   (ensure-worker-src! pdfjs)
                                   (resolve pdfjs))
                               (reject (js/Error. "PDF.js GlobalWorkerOptions missing")))
                             (reject (js/Error. "PDF.js failed to initialize")))))
                   (set! (.-onerror script)
                         (fn []
                           (debug-log "pdfjs script failed to load")
                           (reject (js/Error. "Failed to load PDF.js"))))
                   (.appendChild (.-head js/document) script))))]
          (reset! pdfjs-loader promise)
          promise))))

(defn ^:private file-format [file]
  (let [name (str/lower-case (or (.-name file) ""))]
    (cond
      (str/ends-with? name ".md") :markdown
      (str/ends-with? name ".markdown") :markdown
      (str/ends-with? name ".json") :json
      (str/ends-with? name ".pdf") :pdf
      :else nil)))

(defn ^:private read-text [file]
  (.text file))

(defn ^:private extract-pdf-text [pdf page texts resolve]
  (if (> page (aget pdf "numPages"))
    (let [text (str/join "\n" texts)]
      (debug-log "pdf text extracted" #js {:pages (dec page) :chars (count text)})
      (debug-log "pdf text preview" (subs text 0 (min 1000 (count text))))
      (resolve text))
    (-> (.call (aget pdf "getPage") pdf page)
        (.then #(.call (aget % "getTextContent") %))
        (.then
         (fn [content]
           (debug-log "pdf page read" page)
           (let [page-text (->> (aget content "items")
                                (map #(or (aget % "str") ""))
                                (str/join "\n"))]
             (extract-pdf-text pdf (inc page) (conj texts page-text) resolve)))))))

(defn ^:private read-pdf-text [file]
  (debug-log "reading pdf" (file-meta file))
  (-> (load-pdfjs)
      (.then
       (fn [pdfjs]
         (js/Promise.
          (fn [resolve reject]
            (let [reader (js/FileReader.)]
              (set! (.-onload reader)
                    (fn [event]
                      (try
                        (let [get-document (aget pdfjs "getDocument")
                              data (.. event -target -result)]
                          (debug-log "pdf arraybuffer ready" #js {:bytes (.-byteLength data)})
                          (-> (.call get-document pdfjs #js {:data data})
                              (aget "promise")
                              (.then (fn [pdf]
                                       (debug-log "pdf document opened"
                                                  #js {:pages (aget pdf "numPages")})
                                       (extract-pdf-text pdf 1 [] resolve)))
                              (.catch
                               (fn [err]
                                 (debug-log "pdf document error" (.-message err))
                                 (reject err)))))
                        (catch :default e
                          (debug-log "pdf read error" (.-message e))
                          (reject e)))))
              (set! (.-onerror reader)
                    (fn [err]
                      (debug-log "pdf filereader error" err)
                      (reject err)))
              (.readAsArrayBuffer reader file))))))
      (.catch
       (fn [err]
         (debug-log "pdfjs load error" (.-message err))
         (js/Promise.reject err)))))

(defn ^:private parse-text [text format filename]
  (debug-log "parsing" filename #js {:format (name format) :chars (count text)})
  (debug-log "raw text preview" (subs text 0 (min 1000 (count text))))
  (let [result (if (= format :pdf)
                 (parser/parse-pdf-text text)
                 (parser/parse-document text format))]
    (debug-log "parse complete" filename #js {:valid (:valid? result)
                                               :sheet-count (count (:sheets result))
                                               :errors (:errors result)})
    (debug-json (str "result json (" filename ")") result)
    (debug-json (str "sheets json (" filename ")") (:sheets result))
    result))

(defn ^:private process-file [file]
  (let [format (file-format file)
        filename (.-name file)]
    (debug-log "start" (file-meta file) #js {:format (when format (name format))})
    (if (nil? format)
      (let [result {:valid? false :sheets [] :errors ["Unsupported file type"]}]
        (debug-log "unsupported file type" filename)
        (debug-json "result json" result)
        (js/Promise.resolve result))
      (-> (if (= format :pdf)
            (read-pdf-text file)
            (read-text file))
          (.then #(parse-text % format filename))
          (.catch
           (fn [err]
             (debug-log "process-file error" filename (.-message err))
             (js/Promise.reject err)))))))

(defn use-document-importer []
  (let [dispatch (dispatch/use-dispatch)
        publish  (events/use-publish)]
    (uix/use-callback
     (fn [files]
       (debug-log "files selected" #js {:count (.-length files)})
       (doseq [file (array-seq files)]
         (-> (process-file file)
             (.then
              (fn [{:keys [valid? sheets errors]}]
                (if valid?
                  (do (debug-log "dispatching import"
                                 (.-name file)
                                 #js {:sheet-count (count sheets)
                                      :names (clj->js (mapv :name sheets))})
                      (try
                        (dispatch :character-sheets/import sheets (.-name file))
                        (debug-log "import success" (.-name file) #js {:sheets (count sheets)})
                        (publish :import/success {:count (count sheets)
                                                  :names (mapv :name sheets)})
                        (catch :default e
                          (debug-log "dispatch error" (.-name file) (.-message e))
                          (publish :import/error (.-message e) (.-name file)))))
                  (do (debug-log "import failed" (.-name file) (first errors))
                      (publish :import/error (first errors) (.-name file))))))
             (.catch
              (fn [err]
                (let [message (or (.-message err) (str err))]
                  (debug-log "import exception" (.-name file) message)
                  (publish :import/error message (.-name file))))))))
     [dispatch publish])))

(defui ^:private listeners []
  (let [import! (use-document-importer)]
    (events/use-subscribe :import/parse-files
      (uix/use-callback
       (fn [files]
         (import! files)) [import!]))))

(defui provider [props]
  ($ :<>
    ($ listeners)
    (:children props)))
