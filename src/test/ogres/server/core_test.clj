(ns ogres.server.core-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [ogres.server.core :as core])
  (:import [jakarta.websocket RemoteEndpoint$Async Session]))

(defn stub-session
  "Returns a minimal Session that records whether it was closed and can be
   told to answer pings."
  [id {:keys [answers-ping?]}]
  (let [open? (atom true)]
    (reify Session
      (getId [_] id)
      (isOpen [_] (deref open?))
      (close [_] (reset! open? false))
      (close [_ _] (reset! open? false))
      (getAsyncRemote [_]
        (reify RemoteEndpoint$Async
          (sendPing [_ _]
            (when answers-ping? (core/touch! id)))
          (sendText [_ _] nil))))))

(defn reset-state [f]
  (reset! core/state! {})
  (reset! core/seen! {})
  (f))

(use-fixtures :each reset-state)

(deftest room-leave-by-host-forgets-every-member
  (let [data (-> {}
                 (core/room-create "ABCD" "host" :host-session)
                 (core/room-join "ABCD" "p1" :p1-session)
                 (core/room-join "ABCD" "p2" :p2-session))
        left (core/room-leave data "host")]
    (is (= {} (:rooms left)))
    (is (= {} (:conns left)))))

(deftest room-leave-by-player-keeps-room
  (let [data (-> {}
                 (core/room-create "ABCD" "host" :host-session)
                 (core/room-join "ABCD" "p1" :p1-session))
        left (core/room-leave data "p1")]
    (is (= #{"host"} (get-in left [:rooms "ABCD" :conns])))
    (is (nil? (get-in left [:conns "p1"])))))

(deftest room-join-after-room-closed-is-ignored
  (let [data (-> {}
                 (core/room-create "ABCD" "old-host" :old-session)
                 (core/room-leave "old-host")
                 (core/room-join "ABCD" "p1" :p1-session)
                 (core/room-create "ABCD" "new-host" :new-session))]
    (is (= {"ABCD" {:conns #{"new-host"} :host "new-host"}} (:rooms data)))
    (is (nil? (get-in data [:conns "p1"])))))

(deftest late-player-close-does-not-touch-recreated-room
  (let [data (-> {}
                 (core/room-create "ABCD" "old-host" :old-session)
                 (core/room-join "ABCD" "p1" :p1-session)
                 (core/room-leave "old-host")
                 (core/room-create "ABCD" "new-host" :new-session)
                 (core/room-leave "p1"))]
    (is (= {"ABCD" {:conns #{"new-host"} :host "new-host"}} (:rooms data)))
    (is (= [] (core/uuid->conns data "p1")))))

(deftest reclaim-room-refuses-responsive-host
  (let [host (stub-session "host" {:answers-ping? true})]
    (swap! core/state! core/room-create "ABCD" "host" host)
    (is (= {:status 403} (core/reclaim-room "ABCD")))
    (is (= "host" (get-in @core/state! [:rooms "ABCD" :host])))
    (is (.isOpen host))))

(deftest reclaim-room-evicts-silent-host
  (with-redefs [core/reclaim-probe-ms 200]
    (let [host   (stub-session "host" {:answers-ping? false})
          player (stub-session "p1" {:answers-ping? true})]
      (swap! core/state! core/room-create "ABCD" "host" host)
      (swap! core/state! core/room-join "ABCD" "p1" player)
      (is (nil? (core/reclaim-room "ABCD")))
      (is (= {} (:rooms @core/state!)))
      (is (not (.isOpen player)) "players are closed so they auto-rejoin")
      (is (nil? (core/handle-ws {:params {:host "ABCD"}}))))))

(deftest sweep-drops-only-silent-connections
  (let [host   (stub-session "host" {:answers-ping? true})
        player (stub-session "p1" {:answers-ping? false})
        stale  (- (System/currentTimeMillis) core/liveness-timeout-ms 1000)]
    (swap! core/state! core/room-create "ABCD" "host" host)
    (swap! core/state! core/room-join "ABCD" "p1" player)
    (reset! core/seen! {"host" stale "p1" stale})
    (testing "a silent host destroys the room and frees its code"
      (core/sweep!)
      (is (= {} (:rooms @core/state!)))
      (is (= {} (:conns @core/state!)))
      (is (= {} @core/seen!)))))

(deftest sweep-pings-live-connections
  (let [host (stub-session "host" {:answers-ping? true})]
    (swap! core/state! core/room-create "ABCD" "host" host)
    (reset! core/seen! {"host" (- (System/currentTimeMillis) 1000)})
    (core/sweep!)
    (is (= "host" (get-in @core/state! [:rooms "ABCD" :host])))
    (is (> (get @core/seen! "host") (- (System/currentTimeMillis) 500)))))
