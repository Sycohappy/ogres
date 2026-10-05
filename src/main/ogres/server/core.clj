(ns ogres.server.core
  (:gen-class)
  (:refer-clojure :exclude [send])
  (:import [clojure.lang IPersistentMap]
           [jakarta.websocket CloseReason CloseReason$CloseCodes MessageHandler$Whole PongMessage Session]
           [java.io ByteArrayOutputStream ByteArrayInputStream]
           [java.nio ByteBuffer]
           [java.util.concurrent Executors TimeUnit]
           [org.msgpack.core MessagePack])
  (:require [clojure.string :refer [upper-case]]
            [cognitect.transit :as transit]
            [datascript.core]
            [datascript.transit :refer [read-handlers write-handlers]]
            [io.pedestal.connector :as conn]
            [io.pedestal.http.jetty :as jetty]
            [io.pedestal.log :as log]
            [io.pedestal.metrics :as metrics]
            [io.pedestal.websocket :as ws]))

(def state! (atom {}))

;; Epoch milliseconds of the last frame (message or pong) received from each
;; connection, keyed by connection uuid.
(def seen! (atom {}))

;; A browser socket can die without the close ever reaching the server, which
;; leaves a host registered whose room can never be reclaimed. Connections are
;; pinged on an interval and dropped when they stop answering.
(def ping-interval-ms 15000)
(def liveness-timeout-ms 40000)
(def reclaim-probe-ms 5000)

(def opts-reader {:handlers read-handlers})
(def opts-writer {:handlers write-handlers})

(metrics/gauge
 :ogres.server.conns/count
 {::metrics/description "The number of connections persisted in state."}
 (fn [] (count (:conns (deref state!)))))

(metrics/gauge
 :ogres.server.rooms/count
 {::metrics/description "The number of rooms persisted in state."}
 (fn [] (count (:rooms (deref state!)))))

(def stat-size-message!
  (metrics/histogram
   :ogres.server.message/size
   {::metrics/description "The size of an event forward to one or more connections."
    ::metrics/unit "By"}))

(def stat-size-image!
  (metrics/histogram
   :ogres.server.image/size
   {::metrics/description "The size of an image forwarded to another connection."
    ::metrics/unit "By"}))

(defn room-create-key []
  (let [keys (:rooms (deref state!))]
    (loop []
      (let [code (->> (datascript.core/squuid) (str) (take-last 4) (apply str) (upper-case))]
        (if (contains? keys code) (recur) code)))))

(defn room-create [data room uuid session]
  ;; Concurrent requests for the same room code can all pass the upgrade
  ;; check; only the first to open becomes its host.
  (if (get-in data [:rooms room :host])
    data
    (-> data
        (update-in [:conns uuid] assoc :session session :room room)
        (update-in [:rooms room] assoc :conns #{uuid} :host uuid))))

(defn room-join [data room uuid session]
  ;; The room may have closed between the upgrade check and the socket
  ;; opening; joining it then would create a room without a host.
  (if (get-in data [:rooms room :host])
    (-> data
        (update-in [:conns uuid] assoc :session session :room room)
        (update-in [:rooms room :conns] conj uuid))
    data))

(defn room-leave [data uuid]
  (let [room (get-in data [:conns uuid :room])
        host (get-in data [:rooms room :host])]
    (cond (nil? host)
          (update data :conns dissoc uuid)

          ;; The host has left; forget every member of the room so that late
          ;; close events from its players cannot reach a new room that has
          ;; since been created with the same code.
          (= uuid host)
          (-> (apply update data :conns dissoc uuid (get-in data [:rooms room :conns]))
              (update :rooms dissoc room))

          :else
          (-> data
              (update :conns dissoc uuid)
              (update-in [:rooms room :conns] disj uuid)))))

(defn uuid->room [data uuid]
  (let [room (get-in data [:conns uuid :room])]
    (get-in data [:rooms room])))

(defn uuid->conns [data uuid]
  (let [room  (get-in data [:conns uuid :room])
        uuids (get-in data [:rooms room :conns])]
    (into [] (comp (map (:conns data)) (map :session))
          (disj uuids uuid))))

(defn encode [value]
  (let [stream (ByteArrayOutputStream.)
        writer (transit/writer stream :json opts-writer)]
    (transit/write writer value)
    (.toString stream)))

(defn send [session message]
  (when (.isOpen session)
    (condp instance? message
      String
      (.sendText (.getAsyncRemote session) message)
      ByteBuffer
      (.sendBinary (.getAsyncRemote session) message)
      IPersistentMap
      (.sendText (.getAsyncRemote session) (encode message)))))

(defn send-many [sessions message]
  (condp instance? message
    String
    (doseq [session sessions :when (.isOpen session)]
      (.sendText (.getAsyncRemote session) message))
    ByteBuffer
    (doseq [session sessions :when (.isOpen session)]
      (.sendBinary (.getAsyncRemote session) message))
    IPersistentMap
    (let [serialized (encode message)]
      (doseq [session sessions :when (.isOpen session)]
        (.sendText (.getAsyncRemote session) serialized)))))

(defn touch! [uuid]
  (swap! seen! assoc uuid (System/currentTimeMillis)))

(defn ping! [^Session session]
  (try
    (when (.isOpen session)
      (.sendPing (.getAsyncRemote session) (ByteBuffer/allocate 0)))
    (catch Exception error
      (log/warn :message "failed to ping connection" :uuid (.getId session) :error (.getMessage error)))))

(defn close-async! [^Session session reason]
  (future
    (try
      (when (.isOpen session)
        (.close session (CloseReason. CloseReason$CloseCodes/GOING_AWAY reason)))
      (catch Exception error
        (log/warn :message "failed to close connection" :uuid (.getId session) :error (.getMessage error))))))

(defn disconnect!
  "Removes the connection from state. When it is the host of a room, the room
   is destroyed and its remaining connections are closed; otherwise the rest
   of the room is notified that it has left."
  [uuid]
  (let [data  (deref state!)
        room  (uuid->room data uuid)
        conns (uuid->conns data uuid)]
    (if (= (:host room) uuid)
      (doseq [session conns :when (.isOpen session)]
        (.close session))
      (send-many conns {:type :event :data {:name :session/leave :uuid uuid}}))
    (let [[before after] (swap-vals! state! room-leave uuid)
          removed (remove (:conns after) (keys (:conns before)))]
      (swap! seen! #(apply dissoc % uuid removed)))))

(defn responsive?
  "Pings the connection and waits briefly for any frame in return."
  [uuid ^Session session]
  (let [since (System/currentTimeMillis)]
    (ping! session)
    (loop []
      (cond (>= (get (deref seen!) uuid 0) since) true
            (> (- (System/currentTimeMillis) since) reclaim-probe-ms) false
            :else (do (Thread/sleep 100) (recur))))))

(defn sweep! []
  (let [now (System/currentTimeMillis)]
    (doseq [[uuid {session :session}] (:conns (deref state!))
            :when (contains? (:conns (deref state!)) uuid)]
      (if (> (- now (get (deref seen!) uuid now)) liveness-timeout-ms)
        (do (log/warn :message "dropping unresponsive connection" :uuid uuid)
            (disconnect! uuid)
            (close-async! session "unresponsive"))
        (ping! session)))))

(defn start-liveness! []
  (doto (Executors/newSingleThreadScheduledExecutor)
    (.scheduleWithFixedDelay
     ^Runnable
     (fn []
       (try (sweep!)
            (catch Throwable error
              (log/error :message "liveness sweep failed" :error (.getMessage error)))))
     ping-interval-ms ping-interval-ms TimeUnit/MILLISECONDS)))

(defn handle-root [_]
  {:status 405})

(defn reclaim-room
  "Lets a host reopen a room whose previous host connection has gone silent.
   Returns a 403 response while the existing host still answers, which keeps a
   second tab from taking over a live room."
  [room]
  (let [data    (deref state!)
        uuid    (get-in data [:rooms room :host])
        session (get-in data [:conns uuid :session])]
    (if (and session (.isOpen ^Session session) (responsive? uuid session))
      {:status 403}
      (do (log/warn :message "reclaiming room from unresponsive host" :room room :uuid uuid)
          (disconnect! uuid)
          (when session (close-async! session "replaced"))
          nil))))

(defn handle-ws [{{host :host join :join} :params}]
  (let [data (deref state!)]
    (cond (and host join)
          {:status 400}
          (and host (get-in data [:rooms host]))
          (reclaim-room host)
          (and join (nil? (get-in data [:rooms (upper-case join)])))
          {:status 404})))

(defn handle-ws-open [session _]
  (.setMaxTextMessageBufferSize   session 1e7)
  (.setMaxBinaryMessageBufferSize session 1e7)
  (let [params (.getRequestParameterMap session)
        host (some-> params (.get "host") (.get 0))
        join (some-> params (.get "join") (.get 0) (upper-case))
        uuid (.getId session)]
    (touch! uuid)
    (.addMessageHandler
     ^Session session
     PongMessage
     ^MessageHandler$Whole
     (reify MessageHandler$Whole
       (onMessage [_ _] (touch! uuid))))
    (cond (some? host)
          (let [data (swap! state! room-create host uuid session)]
            (if (= uuid (get-in data [:rooms host :host]))
              (send session {:type :event :src uuid :dst uuid :data {:name :session/created :room host :uuid uuid}})
              (close-async! session "room already hosted")))
          (some? join)
          (let [data (swap! state! room-join join uuid session)]
            (if (contains? (:conns data) uuid)
              (do (send session {:type :event :src uuid :dst uuid :data {:name :session/joined :room join :uuid uuid}})
                  (send-many (uuid->conns data uuid) {:type :event :src uuid :data {:name :session/join :room join :uuid uuid}}))
              (close-async! session "room closed")))
          :else
          (let [room (room-create-key)
                data (swap! state! room-create room uuid session)]
            (if (= uuid (get-in data [:rooms room :host]))
              (send session {:type :event :src uuid :dst uuid :data {:name :session/created :room room :uuid uuid}})
              (close-async! session "room code collision"))))
    session))

(defn handle-ws-close [session _ _]
  ;; The connection has been closed; close the associated session.
  (when (.isOpen session)
    (.close session))
  (disconnect! (.getId session)))

(defn handle-ws-error [_ _ error]
  (log/error :message (.getMessage error)))

(defn handle-ws-text [session message]
  (stat-size-message! (.length message))
  (let [data (deref state!)
        uuid (.getId session)]
    (touch! uuid)
    (if (uuid->room data uuid)
      (let [stream (ByteArrayInputStream. (.getBytes message))
            reader (transit/reader stream :json opts-reader)
            decode (transit/read reader)]
        (if-let [uuid (:dst decode)]
          (send      (get-in data [:conns uuid :session]) message)
          (send-many (uuid->conns data uuid) message))))))

(defn handle-ws-binary [session message]
  (stat-size-image! (.remaining message))
  (let [data (deref state!)
        uuid (.getId session)]
    (touch! uuid)
    (if (uuid->room data uuid)
      (let [unpacker (MessagePack/newDefaultUnpacker message)
            max-keys (.unpackMapHeader unpacker)]
        (loop [idx 0]
          (if (< idx max-keys)
            (if (= (.unpackString unpacker) "dst")
              (let [dest (.unpackString unpacker)]
                (when-let [session (get-in data [:conns dest :session])]
                  (send session message)))
              (do (.skipValue unpacker)
                  (recur (inc idx))))))
        (.close unpacker)))))

(def upgrade-ws
  (ws/websocket-upgrade
   {:on-open   handle-ws-open
    :on-close  handle-ws-close
    :on-error  handle-ws-error
    :on-text   handle-ws-text
    :on-binary handle-ws-binary
    :idle-timeout-ms (* 1000 60 3)}))

(defn create-connector
  ([] (create-connector {}))
  ([{:keys [port] :or {port 5000}}]
   (-> (conn/default-connector-map "0.0.0.0" port)
       (conn/with-default-interceptors)
       (conn/with-routes
         #{["/"   :get [handle-root]]
           ["/ws" :get [handle-ws upgrade-ws]]})
       (jetty/create-connector nil))))

(defn -main [port]
  (start-liveness!)
  (conn/start! (create-connector {:port (Integer/parseInt port)})))
