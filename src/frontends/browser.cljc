(ns frontends.browser
  (:require ["socket.io-client" :as socket-io]
            [allpa.core :as a]
            [accountant.core :as accountant]
            [mayu.attach :as attach]
            [mayu.frp.event :as e]
            [mayu.frp.signal :as s]
            [liverpool :as l]
            [router :as r]
            [cognitect.transit :as t]
            [ui.entry]))

(def writer (a/writer :json))
(def reader (a/reader :json))

(defonce a-s-route (atom {}))
(defonce a-off (atom (fn [])))

(defonce a-e-server (atom (e/Event)))
(defonce a-emit (atom (fn [])))

(defn mount-root []
  (let [{:keys [off]}
        (attach/attach (js/document.getElementById "app")
                       {:get-href #(.. js/window -location -href)
                        :get-hash #(.. js/window -location -hash)
                        :set-hash #(set! (.. js/window -location -hash) %)
                        :e-server @a-e-server
                        :emit @a-emit
                        ::r/s-route @a-s-route} ui.entry/root)]
    (reset! a-off off)))

(defn main! []
  (println "Client init")
  (let [e-server (e/on! (e/Event))
        socket (socket-io "")
        e-route (e/on! (e/Event))
        {s-route :signal} (s/build (s/from nil e-route))]
    (reset! a-e-server e-server)
    (reset! a-emit
            (fn [action]
              (.emit socket "liverpool"
                     (t/write writer action))))
    (.on socket "connect" #(println "Connected to server"))
    (.on socket "liverpool"
         #(e/push! e-server (t/read reader %1)))

    (reset! a-s-route s-route)
    (accountant/configure-navigation!
     {:nav-handler #(e/push! e-route (r/match-by-path %1))
      :path-exists? #(boolean (r/match-by-path %1))})
    (accountant/dispatch-current!)
    (mount-root)))

(defn reload! []
  (println "reloading")
  (@a-off)
  (mount-root)
  (let [room-id (->> (.. js/window -location -hash)
                     (drop 1)
                     (reduce str ""))]
    (@a-emit (l/->Ping room-id))))
