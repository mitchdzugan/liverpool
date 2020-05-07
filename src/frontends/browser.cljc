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

(defn mount-root []
  (let [{:keys [off]}
        (attach/attach (js/document.getElementById "app")
                       {::r/s-route @a-s-route} ui.entry/root)]
    (reset! a-off off)))

(defn main! []
  (println "Client init")
  (let [socket (socket-io "")
        e-route (e/on! (e/Event))
        {s-route :signal} (s/build (s/from nil e-route))]
    (.on socket "connect" #(println "Connected to server"))
    (.on socket "liverpool"
         #(js/console.log (t/read reader %1)))
    (.emit socket "liverpool"
           (t/write writer (l/->CreateRoom "Mitch")))
    (reset! a-s-route s-route)
    (accountant/configure-navigation!
     {:nav-handler #(e/push! e-route (r/match-by-path %1))
      :path-exists? #(boolean (r/match-by-path %1))})
    (accountant/dispatch-current!)
    (mount-root)))

(defn reload! []
  (println "reloading")
  (@a-off)
  (mount-root))
