(ns ogres.app.import.parser
  (:require [clojure.string :as str]))

(def ^:private size-codes
  {"tiny" "T" "small" "S" "medium" "M" "large" "L" "huge" "H" "gargantuan" "G"})

(def ^:private alignment-codes
  {"lawful" "L" "neutral" "N" "chaotic" "C" "good" "G" "evil" "E" "unaligned" "U"})

(defn ^:private strip-blockquote [line]
  (str/trim (str/replace line #"^>\s?" "")))

(defn ^:private extract-stat-blocks [text]
  (let [lines (str/split-lines text)]
    (loop [blocks [] current [] lines lines]
      (if (empty? lines)
        (if (seq current) (conj blocks (vec current)) blocks)
        (let [line (first lines)
              bq?  (str/starts-with? (str/trim line) ">")]
          (if bq?
            (recur blocks (conj current line) (rest lines))
            (recur (if (seq current) (conj blocks (vec current)) blocks)
                   []
                   (rest lines))))))))

(defn ^:private parse-size-type-alignment [line]
  (when-let [m (re-find #"\*(.+?),\s*(.+?)\*" line)]
    (let [[_ size-type alignment] m
          parts (str/split (str/trim size-type) #"\s+" 2)
          size  (when (seq parts) (get size-codes (str/lower-case (first parts))))
          type  (when (> (count parts) 1) (str/lower-case (second parts)))
          align (mapv #(get alignment-codes (str/lower-case %))
                      (str/split (str/trim alignment) #"\s+"))]
      {:size (when size [size])
       :type type
       :alignment (vec (remove nil? align))})))

(defn ^:private parse-hp [value]
  (when-let [m (re-find #"(\d+)\s*\(([^)]+)\)" value)]
    {:average (js/parseInt (nth m 1))
     :formula (str/trim (nth m 2))}))

(defn ^:private parse-speed [value]
  (let [parts (str/split value #",\s*")]
    (into {}
          (keep
           (fn [part]
             (cond
               (re-find #"^(\d+)\s*ft\.?$" (str/trim part))
               (let [[_ n] (re-find #"^(\d+)\s*ft\.?$" (str/trim part))]
                 [:walk (js/parseInt n)])

               (re-find #"^(\w+)\s+(\d+)\s*ft\.?$" (str/trim part))
               (let [[_ mode n] (re-find #"^(\w+)\s+(\d+)\s*ft\.?$" (str/trim part))]
                 [(keyword (str/lower-case mode)) (js/parseInt n)])

               :else nil))
          parts))))

(defn ^:private parse-skills [value]
  (into {}
        (for [skill (str/split value #",\s*")
              :let [m (re-find #"^(\w+)\s+([+-]\d+)$" (str/trim skill))]
              :when m]
          [(keyword (str/lower-case (nth m 1))) (nth m 2)])))

(defn ^:private parse-list [value]
  (vec (map str/trim (str/split value #",\s*"))))

(defn ^:private parse-ac [value]
  (when-let [n (re-find #"(\d+)" value)]
    [(js/parseInt (second n))]))

(defn ^:private parse-cr [value]
  (when-let [m (re-find #"^([\d/]+)" (str/trim value))]
    (nth m 1)))

(defn ^:private parse-passive [value]
  (when-let [m (re-find #"Passive Perception\s+(\d+)" value)]
    (js/parseInt (second m))))

(defn ^:private parse-ability-scores [lines]
  (let [table-lines (filter #(re-find #"\|\s*\d+" %) lines)]
    (when (seq table-lines)
      (let [row (first (filter #(re-find #"\d+\s*\([+-]?\d+\)" %) table-lines))
            scores (when row (re-seq #"(\d+)\s*\([+-]?\d+\)" row))]
        (when (= (count scores) 6)
          (zipmap [:str :dex :con :int :wis :cha]
                  (map #(js/parseInt (second %)) scores)))))))

(defn ^:private parse-named-entries [lines]
  (into []
        (keep
         (fn [line]
           (when-let [m (re-find #"\*\*\*(.+?)\.\*\*\*\s*(.*)" line)]
             {:name (str/trim (nth m 1))
              :entries [(str/trim (nth m 2))]}))
         lines)))

(defn ^:private parse-stat-line [line]
  (when-let [m (re-find #"-\s*\*\*(.+?)\*\*\s+(.+)" line)]
    (let [key (str/lower-case (str/replace (nth m 1) #" " ""))
          val (str/trim (nth m 2))]
      (case key
        "armorclass" [:ac (parse-ac val)]
        "hitpoints"  [:hp (parse-hp val)]
        "speed"      [:speed (parse-speed val)]
        "initiative" [:initiative {:bonus (first (re-seq #"[+-]\d+" val))}]
        "skills"     [:skill (parse-skills val)]
        "immunities" [:immune (mapv str/lower-case (parse-list val))]
        "resistances" [:resist (mapv str/lower-case (parse-list val))]
        "vulnerabilities" [:vulnerable (mapv str/lower-case (parse-list val))]
        "senses"     [:senses (parse-list val)]
        "languages"  [:languages (parse-list val)]
        "challenge"  [:cr (parse-cr val)]
        "proficiencybonus" [:proficiency-bonus val]
        "habitat"    [:environment (mapv str/lower-case (parse-list val))]
        "treasure"   [:treasure (mapv str/lower-case (parse-list val))]
        nil))))

(defn ^:private parse-block [block-lines]
  (let [lines (mapv strip-blockquote block-lines)
        name  (some (fn [l] (when-let [m (re-find #"^##\s+(.+)" l)] (str/trim (second m)))) lines)
        subtitle (some #(when (str/starts-with? % "*") %) lines)
        meta  (when subtitle (parse-size-type-alignment subtitle))
        stats (into {} (keep parse-stat-line) lines)
        abilities (parse-ability-scores lines)
        action-start (first (keep-indexed #(when (re-find #"^###\s+Actions" %2) %1) lines))
        action-lines (if action-start (subvec lines (inc action-start)) [])
        legendary-start (first (keep-indexed #(when (re-find #"^###\s+Legendary Actions" %2) %1) action-lines))
        action-only-lines (if legendary-start
                            (subvec action-lines 0 legendary-start)
                            action-lines)
        legendary-lines (if legendary-start
                          (subvec action-lines (inc legendary-start))
                          [])
        trait-entries (parse-named-entries (if action-start (subvec lines 0 action-start) lines))
        action-entries (parse-named-entries action-only-lines)
        legendary-entries (parse-named-entries legendary-lines)
        passive (some parse-passive (:senses stats))]
    (when name
      (cond-> {:name name}
        meta (merge (select-keys meta [:size :type :alignment]))
        (:ac stats) (assoc :ac (:ac stats))
        (:hp stats) (assoc :hp (:hp stats))
        (:speed stats) (assoc :speed (:speed stats))
        (:initiative stats) (assoc :initiative (:initiative stats))
        abilities (merge abilities)
        (:skill stats) (assoc :skill (:skill stats))
        (:senses stats) (assoc :senses (:senses stats))
        passive (assoc :passive passive)
        (:immune stats) (assoc :immune (:immune stats))
        (:resist stats) (assoc :resist (:resist stats))
        (:vulnerable stats) (assoc :vulnerable (:vulnerable stats))
        (:languages stats) (assoc :languages (:languages stats))
        (:cr stats) (assoc :cr (:cr stats))
        (:environment stats) (assoc :environment (:environment stats))
        (:treasure stats) (assoc :treasure (:treasure stats))
        (seq trait-entries) (assoc :trait trait-entries)
        (seq action-entries) (assoc :action action-entries)
        (seq legendary-entries) (assoc :legendary legendary-entries)
        true (assoc :hasToken true :hasFluff true)))))

(defn validate-sheet [sheet]
  (let [errors (cond-> []
                 (not (string? (:name sheet))) (conj "name must be a string")
                 (not (seq (:name sheet))) (conj "name is required")
                 (and (contains? sheet :ac) (not (vector? (:ac sheet)))) (conj "ac must be a vector")
                 (and (contains? sheet :hp)
                      (not (and (map? (:hp sheet))
                                (number? (:average (:hp sheet)))
                                (string? (:formula (:hp sheet))))))
                 (conj "hp must have average and formula")
                 (and (contains? sheet :str) (not (number? (:str sheet)))) (conj "str must be a number"))]
    (if (seq errors)
      {:valid? false :errors errors :sheet nil}
      {:valid? true :errors [] :sheet sheet})))

(defn parse-markdown [text]
  (let [blocks (extract-stat-blocks text)
        sheets (into [] (keep parse-block) blocks)]
    (if (seq sheets)
      {:valid? true :sheets sheets :errors []}
      {:valid? false :sheets [] :errors ["No stat blocks found in markdown"]})))

(defn parse-json-text [text]
  (try
    (let [data (js/JSON.parse text)
          sheets (if (array? data) (vec (array-seq data)) [data])
          results (mapv validate-sheet sheets)]
      (if (every? :valid? results)
        {:valid? true :sheets (mapv :sheet results) :errors []}
        {:valid? false :sheets [] :errors (mapcat :errors results)}))
    (catch :default e
      {:valid? false :sheets [] :errors [(.-message e)]})))

(defn parse-document [text format]
  (case format
    :markdown (parse-markdown text)
    :json (parse-json-text text)
    {:valid? false :sheets [] :errors [(str "Unsupported format: " format)]}))
