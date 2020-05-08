(ns ui.entry
  (:require ["clipboard-copy" :as copy]
            [mayu.macros :refer [defui ui]]
            [mayu.frp.event :as e]
            [mayu.frp.signal :as s]
            [mayu.dom :as dom]
            [router :as r]
            [liverpool :as l]
            [card :as c]))

(defui emit-action [e-action]
  emit <- (dom/envs :emit)
  (dom/consume! e-action emit))

(defui emit-game-action [e-action-args]
  state <- (dom/envs :state)
  let [{:keys [room-id]} state]
  (->> e-action-args
       (e/map (fn [[cons & args]]
                (apply cons room-id args)))
       emit-action))

(defn get-screen [state]
  (cond
    (nil? state) :home
    (:started? state) :play
    :else :config))

(defui home-screen []
  state <- (dom/envs :state)
  get-hash <- (dom/envs :get-hash)
  <[dom/collect-reduce-and-bind
    ::home-state
    (fn [curr [k v]] (assoc curr k v))
    {:name "" :room-id (reduce str "" (drop 1 (get-hash)))}
    $[{:keys [name room-id]}]=
    let [can-create? (> (count name) 2)
         can-join? (and can-create?
                        (= (count room-id) 6))]
    <[div {:class "container content"} $=
      <[div {:class "splash"} $=
        <[div {:class "header"} $=
          <[div {:class "quarantine"} "QUARANTINE"]
          <[div {:class "liverpool"} "Liverpool"]]
        <[div {:class "card-loop-top"} $=
          <[div "card loop"]]
        <[div {:class "actions"} $=
          <[div {:class "field has-addons"} $=
            <[p {:class "control"} $=
              <[input {:class "input"
                       :type "text"
                       :value name
                       :placeholder "Your Name"}
                ] d-name >
              (->> (dom/on-input d-name)
                   (e/map #(.. % -target -value))
                   (e/map #(-> [:name %]))
                   (dom/emit ::home-state))]
            <[p {:class "control"} $=
              <[button {:disabled (not can-create?)
                        :class "button is-danger"}
                "Create Room"] d-create-room >
              (->> (dom/on-click d-create-room)
                   (e/map #(l/->CreateRoom name))
                   emit-action)]]
          <[div {:class "field has-addons"} $=
            <[p {:class "control"} $=
              <[input {:class "input"
                       :type "text"
                       :value room-id
                       :placeholder "Room ID"}
                ] d-room-id >
              (->> (dom/on-input d-room-id)
                   (e/map #(.. % -target -value))
                   (e/map #(-> [:room-id %]))
                   (dom/emit ::home-state))]
            <[p {:class "control"} $=
              <[button {:disabled (not can-join?)
                        :class "button is-danger"}
                "Join Room"] d-join-room >
              (->> (dom/on-click d-join-room)
                   (e/map #(l/->JoinRoom room-id name))
                   emit-action)]]]]]])

(defui play-screen []
  <[p "PLAY"])

(defui config-screen []
  state <- (dom/envs :state)
  get-href <- (dom/envs :get-href)
  let [{:keys [room-id id-by-name config]} state
       {:keys [num-decks]} config]
  <[div {:class "card waiting-room"} $=
    <[div {:class "card-header"} $=
      <[div {:class "tags has-addons are-medium"} $=
        <[span {:class "tag is-success"} "Room ID"]
        <[span {:class "tag is-info has-text-weight-bold"}
          room-id]
        <[span {:class "tag l0nk is-success"} "Copy"] d-copy-id >
        (dom/consume! (dom/on-click d-copy-id)
                      #(copy room-id))]
      <[a {:class "is-small"} "Copy invite link"] d-copy-link >
      (dom/consume! (dom/on-click d-copy-link)
                    #(copy (get-href)))]
    <[div {:class "card-content"} $=
      <[div {:class "content"} $=
        <[table {:class "table is-bordered"} $=
          <[thead $=
            <[tr $=
              <[th "Players In Game"]]]
          <[tbody $=
            <[for (keys id-by-name) $[name]=
              <[keyed name
                <[tr $= <[td name]]]]]]
        <[div {:class "set-decks"} $=
          <[div {:class "field has-addons"} $=
            <[p {:class "control"} $=
              <[a {:class "button is-static"} "Decks"]]
            <[p {:class "control"} $=
              <[a {:class "button is-static"} num-decks]]
            <[p {:class "control"} $=
              <[a {:disabled (= num-decks 1)
                   :class "button"} $=
                <[i {:class "fas fa-arrow-circle-down"}]
                ] d-down >
              (->> (dom/on-click d-down)
                   (e/map #(-> [l/->ConfigureRoom
                                {:num-decks (dec num-decks)}]))
                   emit-game-action)]
            <[p {:class "control"} $=
              <[a {:class "button"} $=
                <[i {:class "fas fa-arrow-circle-up"}]
                ] d-up >
              (->> (dom/on-click d-up)
                   (e/map #(-> [l/->ConfigureRoom
                                {:num-decks (inc num-decks)}]))
                   emit-game-action)]]]]]
    <[div {:class "card-footer"} $=
      <[button {:disabled (= (count id-by-name) 1)
                :class "card-footer-item"}
        "Start Game!"] d-start >
      (->> (dom/on-click d-start)
           (e/map #(-> [l/->StartGame]))
           emit-game-action)]])

(defui root []
  set-hash <- (dom/envs :set-hash)
  e-server <- (dom/envs :e-server)
  let [e-game-state (->> e-server
                         (e/remove l/error?)
                         (e/map :state))
       e-error (->> e-server
                    (e/filter l/error?)
                    (e/map :error))]
  s-error <- (s/from nil e-error)
  s-game-state <- (s/from nil e-game-state)
  <[dom/bind s-game-state $[state]=
    [(js/console.log state)]
    <[dom/assoc-env :state state $=
      <[when (:room-id state)
        [(set-hash (:room-id state))]]
      <[case (get-screen state)
        <[:home <[home-screen]]
        <[:play <[play-screen]]
        <[:config <[config-screen]]]]])
