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

(defn ^:private load-pdfjs []
  (if-let [pdfjs (aget js/window "pdfjsLib")]
    (js/Promise.resolve pdfjs)
    (or @pdfjs-loader
        (let [promise
              (js/Promise.
               (fn [resolve reject]
                 (let [script (.createElement js/document "script")]
                   (set! (.-src script) pdfjs-url)
                   (set! (.-async script) true)
                   (set! (.-onload script)
                         (fn []
                           (if-let [pdfjs (aget js/window "pdfjsLib")]
                             (do
                               (set! (.. pdfjs -GlobalWorkerOptions -workerSrc) pdfjs-worker-url)
                               (resolve pdfjs))
                             (reject (js/Error. "PDF.js failed to initialize")))))
                   (set! (.-onerror script)
                         (fn [] (reject (js/Error. "Failed to load PDF.js"))))
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
  (if (> page (.-numPages pdf))
    (resolve (str/join "\n" texts))
    (-> (.getPage pdf page)
        (.then #(.getTextContent %))
        (.then
         (fn [content]
           (let [page-text (->> (.-items content)
                                (map #(.-str %))
                                (str/join " "))]
             (extract-pdf-text pdf (inc page) (conj texts page-text) resolve)))))))

(defn ^:private read-pdf-text [file]
  (-> (load-pdfjs)
      (.then
       (fn [pdfjs]
         (js/Promise.
          (fn [resolve reject]
            (let [reader (js/FileReader.)]
              (set! (.-onload reader)
                    (fn [event]
                      (try
                        (-> (pdfjs/getDocument #js {:data (.. event -target -result)})
                            (.promise)
                            (.then (fn [pdf] (extract-pdf-text pdf 1 [] resolve)))
                            (.catch reject))
                        (catch :default e
                          (reject e)))))
              (set! (.-onerror reader) reject)
              (.readAsArrayBuffer reader file))))))))

(defn ^:private process-file [file]
  (let [format (file-format file)]
    (if (nil? format)
      (js/Promise.resolve {:valid? false :sheets [] :errors ["Unsupported file type"]})
      (-> (if (= format :pdf)
            (read-pdf-text file)
            (read-text file))
          (.then
           (fn [text]
             (if (= format :pdf)
               (parser/parse-markdown text)
               (parser/parse-document text format))))))))

(defn use-document-importer []
  (let [dispatch (dispatch/use-dispatch)
        publish  (events/use-publish)]
    (uix/use-callback
     (fn [files]
       (doseq [file (array-seq files)]
         (-> (process-file file)
             (.then
              (fn [{:keys [valid? sheets errors]}]
                (if valid?
                  (do (dispatch :character-sheets/import sheets (.-name file))
                      (publish :import/success (count sheets)))
                  (publish :import/error (first errors) (.-name file))))))))
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
