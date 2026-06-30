(ns ogres.app.const)

(goog-define VERSION "latest")
(goog-define PATH "/release")
(goog-define SOCKET-URL "ws://localhost:5000/ws")

(defn resolve-socket-url
  "Returns a browser-reachable WebSocket URL. Build-time SOCKET-URL may
   point at 0.0.0.0 (Docker bind address), which clients cannot connect to."
  []
  (if (and (string? SOCKET-URL) (re-find #"0\.0\.0\.0" SOCKET-URL))
    (str (if (= (.-protocol js/location) "https:") "wss:" "ws:")
         "//" (.-host js/location) "/ws")
    SOCKET-URL))

(def ^:const grid-size
  "The length, in pixels, of a single square in the scene grid. This
   correlates to 5 feet in this spatial system."
  70)

(def ^:const half-size
  "Half the length, in pixels, of a single square in the scene grid."
  35)
