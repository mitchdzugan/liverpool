(ns server
  (:require ["express" :as express]
            ["fs" :as fs]
            ["http" :as http]
            ["socket.io" :as socket-io]
            [allpa.core :as a
             :refer [defprotomethod]]
            [cljs.reader :as reader]
            [router :as r]
            [ui.entry]
            [mayu.dom :as dom]
            [mayu.frp.event :as e]
            [mayu.frp.signal :as s]
            [mayu.async :refer [go-loop <!]]
            [card :as c]
            [liverpool :as l]
            [cognitect.transit :as t]
            [wayra.core :refer [exec]]))

(def writer (a/writer :json))
(def reader (a/reader :json))

(def dev?
  (let [debug? (do ^boolean js/goog.DEBUG)]
    (if debug?
      (cond
        (exists? js/window) true
        (exists? js/process) (not= "true" js/process.env.release)
        :else true)
      false)))

(def assets
  (if dev?
    {:css-name "site.css"
     :output-name "client.js"}
    (-> "./dist/assets.edn"
        (fs/readFileSync "utf8")
        reader/read-string
        first)))

(def css-name (:css-name assets))

(def output-name (:output-name assets))

(def page-pre (str"
<!DOCTYPE html>
<html>
   <head>
      <meta charset=\"utf-8\">
      <meta content=\"width=device-width, initial-scale=1\" name=\"viewport\">
      <link href=\"/" css-name "\" rel=\"stylesheet\" type=\"text/css\">
      <script
          defer
          src=\"https://use.fontawesome.com/releases/v5.3.1/js/all.js\"
      >
      </script>

      <title>liverpool</title>
   </head>
   <body>
      <div id=\"app\">
"))

(def page-post (str"
      </div>
      <script src=\"/" output-name "\" type=\"text/javascript\"></script>
   </body>
</html>
"))

(defn stream-route [res route]
  (.type res "html")
  (.write res page-pre)
  (let [{:keys [signal off]}
        (s/build (s/from route e/never))
        markup-channel (dom/render-to-string {:get-href (fn [] "")
                                              :get-hash (fn [] "")
                                              :set-hash (fn [])
                                              :e-server e/never
                                              :emit (fn [])
                                              ::r/s-route signal} ui.entry/root)]
    (go-loop []
      (let [markup (<! markup-channel)]
        (if (nil? markup)
          (do (.write res page-post)
              (.end res))
          (do (.write res markup)
              (recur)))))))

(def app (express))

(defonce rooms (atom {}))

(defn make-room-id []
  (let [chars "abcdefghijklmnopqrstuvwxyz0123456789"]
    (reduce #(str %1 (nth chars (js/Math.floor (* (count chars)
                                                  (js/Math.random)))))
            ""
            (range 6))))

(defn next-room-id []
  (let [room-id (make-room-id)]
    (if (contains? @rooms room-id)
      (next-room-id)
      room-id)))

(defn broadcast-game-state [room-id]
  (let [{:keys [state socket-by-player]} (get @rooms room-id)]
    (doseq [[name socket] socket-by-player]
      (.emit socket "liverpool"
             (t/write writer (-> state
                                 (l/filter-game-state name)
                                 l/->GameState))))))

(defn use-room [socket room-id f]
  (let [room (get @rooms room-id)]
    (if room
      (f room)
      (.emit socket "liverpool"
             (t/write writer (l/->Error (str "Room " room-id
                                             " does not exist")))))))

(defn handle-game-action [state action socket name]
  (let [{:keys [room-id]} action
        result (exec {:reader {:shuffle shuffle
                               :cards-per-deck 54
                               :name name}
                      :init-state state}
                     (l/handle-game-action action))
        {:keys [error state]} result]
    (if error
      (.emit socket "liverpool"
             (t/write writer (l/->Error error)))
      (do (swap! rooms #(assoc-in %1 [room-id :state] state))
          (broadcast-game-state room-id)))))

(defprotomethod handle-action [action socket]
  !l/CreateRoom
  (let [{:keys [name]} action
        room-id (next-room-id)]
    (swap! rooms
           #(assoc %1
                   room-id
                   {:player-by-socket {socket name}
                    :socket-by-player {name socket}
                    :state {:room-id room-id
                            :config {:num-decks 2}
                            :started? false
                            :id-by-name {name 0}}}))
    (broadcast-game-state room-id))

  !l/JoinRoom
  (let [{:keys [room-id name]} action]
    (use-room socket
              room-id
              #(let [{:keys [state socket-by-player]} %1]
                 (if (contains? socket-by-player name)
                   (swap! rooms
                          (fn [rooms-data]
                            (-> rooms-data
                                (assoc-in [room-id :socket-by-player name]
                                          socket)
                                (update-in [room-id :player-by-socket]
                                           (a/curry dissoc
                                                    (get socket-by-player name)))
                                (assoc-in [room-id :player-by-socket socket]
                                          name))))
                   (do (swap! rooms
                              (fn [rooms-data]
                                (-> rooms-data
                                    (assoc-in [room-id :socket-by-player name]
                                              socket)
                                    (assoc-in [room-id :player-by-socket socket]
                                              name))))
                       (handle-game-action state action socket name))))))

  !l/Ping
  (let [{:keys [room-id]} action]
    (use-room socket room-id
              #(let [{:keys [player-by-socket state]} %1
                     name (get player-by-socket socket)]
                 (.emit socket "liverpool"
                        (t/write writer (-> state
                                            (l/filter-game-state name)
                                            l/->GameState))))))

  [!l/StartGame
   !l/PassDiscard
   !l/RequestDiscard
   !l/Cancel
   !l/Play
   !l/Deal
   !l/ConfigureRoom]
  (let [{:keys [room-id]} action]
    (use-room socket room-id
              #(let [{:keys [state player-by-socket]} %1
                     name (get player-by-socket socket)]
                 (when name
                   (handle-game-action state action socket name))
                 (when (not name)
                   (.emit socket "liverpool"
                          (->> (l/->Error "Player not found in room")
                               (t/write writer))))))))

(defn main! []
  (.get app #".*"
        (fn [req res next]
          (let [url (.-url req)
                route (r/match-by-path url)]
            (if route
              (stream-route res route)
              (next)))))
  (.use app (.static express (if dev? "target" "dist")))
  (.use app (.static express "public"))
  (let [server (http/createServer app)
        io (socket-io server)
        port (if (some? js/process.env.PORT)
               (js/parseInt js/process.env.PORT)
               3000)]
    (.on io "connection"
         (fn [socket]
           (println "New Connection")
           (.on socket "liverpool"
                #(let [action (t/read reader %1)]
                   (handle-action action socket)))))
    (.listen server port #(println (str "App listening on port " port)))))

(defn reload! []
  (println "Code updated."))
