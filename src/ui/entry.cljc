(ns ui.entry
  (:require ["clipboard-copy" :as copy]
            [allpa.core :as a]
            [mayu.macros :refer [defui ui]]
            [mayu.frp.event :as e]
            [mayu.frp.signal :as s]
            [mayu.dom :as dom]
            [wayra.core :refer [exec]]
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

(defui run-m [m]
  state <- (dom/envs :state)
  let [adjusted-state
       (-> state
           (update :hands
                   #(a/map-values (fn [{:keys [held-count] :as hand}]
                                    (if-not (nil? held-count)
                                      (assoc hand :held (range held-count))
                                      hand)))))]
  [(->> m
        (exec {:init-state adjusted-state
               :reader {:cards-per-deck 54}})
        :result)])

(defui passes?-m [m]
  state <- (dom/envs :state)
  let [adjusted-state
       (-> state
           (update :hands
                   #(a/map-values (fn [{:keys [held-count] :as hand}]
                                    (if-not (nil? held-count)
                                      (assoc hand :held (range held-count))
                                      hand))
                                  %1)))]
  [(->> m
        (exec {:init-state adjusted-state
               :reader {:name (:name state)
                        :cards-per-deck 54}})
        :error
        nil?)])

(defui turn-reset [k init]
  state <- (dom/envs :state)
  let [{:keys [turn hand]} state]
  <[dom/memo true $=
    s-game-state <- (dom/envs :s-game-state)
    (->> (s/changed s-game-state)
         (e/map #(-> [(:turn %1) (:hand %1)]))
         (e/filter #(not= % [turn hand]))
         (e/map #(-> init))
         (dom/emit k))])

(defui play-state [inner]
  state <- (dom/envs :state)
  let [crab dom/collect-reduce-and-bind
       {:keys [hands name]} state
       {:keys [may-is held down]} (get hands name)]
  <[crab ::held #(-> %2) held $[held]=
    <[dom/memo held $=
      s-game-state <- (dom/envs :s-game-state)
      (->> (s/changed s-game-state)
           (e/map #(get-in %1 [:hands (:name %1) :held]))
           (e/map (fn [srvr]
                    (let [to-m #(->> %
                                     (a/index-by identity)
                                     (a/map-values (fn [] true)))
                          m-held (to-m held)
                          m-srvr (to-m srvr)]
                      (-> (concat (filter #(get m-srvr %) held)
                                  (remove #(get m-held %) srvr))
                          vec))))
           (dom/emit ::held))]
    <[crab ::tab #(-> %2) :table $[picked-tab]=
      <[crab ::view-table? #(-> %2) false $[view-table?]=
        (turn-reset ::view-table? false)
        <[crab ::plays #(-> %2) {} $[plays]=
          (turn-reset ::plays {})
          <[crab ::selected #(-> %2) nil $[selected-card]=
            <[dom/memo true $=
              s-held <- (dom/envs ::held)
              s-tab <- (dom/envs ::tab)
              s-plays <- (dom/envs ::plays)
              s-deps <- (s/zip-with #(hash [%1 %2 %3]) s-held s-tab s-plays)
              (->> (s/changed s-deps)
                   (e/map #(-> nil))
                   (dom/emit ::selected))]
            <[inner held plays picked-tab selected-card view-table?]]]]]])

(defui unknown-cards [num-cards & args]
  let [opts (nth args 0 {})
       max-cards (get opts :max 11)
       last (:last opts)
       delayed? (:delayed? opts)
       removed? (:removed? opts)
       spread-count (js/Math.min num-cards max-cards)
       width (+ (* 2 spread-count) 25)
       downed-y #(str "translate(" (if % "2px" "0") ", 200%)")
       steady-y #(str "translate(" (if % "2px" "0") ", 0)")
       !ui !rec deck-loop
       (fn [i]
         let [x? (> i (- num-cards spread-count))]
         <[cond
           <[(<= i num-cards)
             <[div {:style {:transition (str "opacity 0.5s ease-in-out, "
                                             "transform 0.5s ease-in-out")
                            :transform (if delayed?
                                         (downed-y x?)
                                         (steady-y x?))
                            :delayed (if delayed?
                                       {:transform (steady-y x?)}
                                       {})
                            :remove (if removed?
                                      {:transform (downed-y x?)}
                                      {})}
                    :class "pcard deck-loop"} $=
               <[img {:src "/cards/15.0.png"}]
               <[deck-loop (inc i)]]]
           <[last
             <[div {:style {:transition "opacity 0.5s ease-in-out 5s"
                            :transform "translate(0, 0)"
                            :opacity "1"
                            :delayed {:opacity "0"}}
                    :class "pcard deck-loop"} $=
               <[img {:src (-> last c/from-int c/to-src)}]]]
           <[:else []]])]
  [(println [:last last])]
  <[div {:style {:width (str width "px")} :class "deck-top"} $=
    <[deck-loop 1]
    ] d-stack >
  <[div {:class "full-count"} num-cards]
  [d-stack])

(defui play-screen []
  prev <- (dom/envs :prev)
  state <- (dom/envs :state)
  hand-winner <- (run-m l/get-hand-winner)
  request-states <- (run-m l/get-request-states)
  let [prev-discard (:discard prev)
       prev-hands (:hands prev)
       {:keys [discard
               deck-count
               first-turn?
               drawn?
               id-by-name
               name-by-id
               hands
               money
               scores
               hand
               dealer
               turn
               discard-requests
               name]} state
       taken? (= (count prev-discard)
                 (inc (count discard)))
       held-count (fn [name hands]
                    (let [{:keys [held held-count]} (get hands name)]
                      (if (nil? held-count) (count held) held-count)))
       taken (and taken? (first prev-discard))
       taker (and taken?
                  (->> id-by-name
                       keys
                       (map #(-> {:name % :diff (- (held-count % hands)
                                                   (held-count % prev-hands))}))
                       (apply max-key :diff)
                       :name))
       players (keys id-by-name)
       final? (= 6 hand)
       round-title (if final? "Final Round" (str "Round " (inc hand)))
       game-over? (and hand-winner final?)
       goals (nth l/hand-goals hand)
       turn-name (get name-by-id turn)
       dealer-name (get name-by-id dealer)
       turn? (= name turn-name)
       id (get name-by-id name)
       hand (get hands name)
       {:keys [may-is held down]} hand
       can-may-i? (and (not drawn?)
                       (not turn?)
                       (> may-is 0))
       passing? (= false (get discard-requests name))
       may-ing? (= true (get discard-requests name))
       may-ier (->> request-states
                    (drop 1)
                    (filter #(= :request (:request-state %)))
                    (map :name)
                    first)
       picking? (and (not hand-winner) turn? (not drawn?))
       intends? (= false (get discard-requests turn-name))
       down? (not (empty? down))]
  [(println {:taker taker})]
  <[play-state $[held plays picked-tab selected-card view-table?]=
    let [!rec leafs (fn [thing]
                      (cond
                        (map? thing) (->> thing
                                          (a/map-values leafs)
                                          vals
                                          flatten)
                        (coll? thing) (mapcat leafs thing)
                        :else thing))
         in-play (->> plays
                      leafs
                      (a/index-by identity)
                      (a/map-values #(-> true)))]
    passes? <- <[dom/memo [round-title plays] $=
                 (passes?-m (l/handle-game-action (l/->Play nil plays)))]
    let [tab (if game-over? :scores picked-tab)
         !ui render-hand
         (fn [parent-id]
           <[div {:class "my-hand"} $=
             <[div {:id parent-id :class "card-parent"} $=
               <[for held $[card]=
                 <[keyed card
                   <[div {:style {:transition (str "opacity 0.5s ease-in-out, "
                                                   "transform 0.5s ease-in-out")
                                  :opacity "0"
                                  :transform "translateX(200%)"
                                  :delayed {:opacity "1"
                                            :transform "translateX(0)"}}
                          :class "pcard" :data-card-val card} $=
                     <[img {:class {:clickable (nil? selected-card)
                                    :selected (= card selected-card)
                                    :in-play (get in-play card)}
                            :src (-> card c/from-int c/to-src)}]
                     ] d-hand >
                   (->> (dom/on-click d-hand)
                        (e/filter #(nil? selected-card))
                        (e/map #(-> card))
                        (dom/emit ::selected))]]]])]
    <[div {:class "in-game"} $=
      <[div {:class "main-controls"} $=
        <[div {:class "field has-addons"} $=
          <[when (not game-over?)
            <[p {:class "control"} $=
              <[button {:style {:color "transparent"}
                        :class "button is-small is-static"}
                name]]
            <[p {:class "control"} $=
              <[button {:class {:button true
                                :is-small true
                                :has-text-weight-bold (= tab :table)}}
                "Table"] d-table >
              (->> (dom/on-click d-table)
                   (e/map #(-> :table))
                   (dom/emit ::tab))]]
          <[p {:class "control"} $=
            <[button {:class {:button true
                              :is-small true
                              :has-text-weight-bold (= tab :scores)}}
              "Scores"] d-scores >
            (->> (dom/on-click d-scores)
                 (e/map #(-> :scores))
                 (dom/emit ::tab))]
          <[p {:class "control"} $=
            <[button {:style (if turn? {:font-weight "bold"} {})
                      :class "button is-small is-static"}
              name]]]
        <[when (= tab :table)
          <[div {:class {:deck true :picking picking? :intended intends?}} $=
            <[when can-may-i?
              <[button {:class {:may-i true :cancel may-ing?}} $=
                <[if may-ing?
                  <[then <[dom/text "Cancel"]]
                  <[else <[dom/text "May I"] <[br] <[dom/text may-is]]]
                ] d-may-i >
              (->> (dom/on-click d-may-i)
                   (e/filter #(-> can-may-i?))
                   (e/map #(-> (if may-ing?
                                 [l/->Cancel]
                                 [l/->RequestDiscard])))
                   emit-game-action)
              <[button {:class {:pass-button true :cancel passing?}}
                (if passing? "Cancel" "Pass")] d-pass >
              (->> (dom/on-click d-pass)
                   (e/filter #(-> can-may-i?))
                   (e/map #(-> (if passing?
                                 [l/->Cancel]
                                 [l/->PassDiscard])))
                   emit-game-action)]
            <[div {:style {:flex "1"}}]
            <[div {:style {:position "relative"}
                   :class "pcard discarded" :data-card-val discard} $=
              <[img {:src (-> nil c/from-int c/to-src)}]
              <[for (reverse discard) $[card]=
                <[keyed card
                  <[div {:style {:position "absolute"
                                 :left "0"
                                 :top "0"
                                 :margin "0"
                                 :transition (str "opacity 0.5s ease-in-out, "
                                                  "transform 0.5s ease-in-out")
                                 :transform "translateY(200%)"
                                 :opacity "0"
                                 :delayed {:transform "translateY(0)"
                                           :opacity "1"}
                                 :remove {:transform "translateY(200%)"
                                          :opacity "0"}}
                         :class "pcard"} $=
                    <[img {:src (-> card c/from-int c/to-src)}]]]]
              ] d-discard >
            <[unknown-cards deck-count {:removed? true}] d-deck >
            (->> (dom/on-click d-discard)
                 (e/map #(-> [l/->RequestDiscard]))
                 emit-game-action)
            (->> (dom/on-click d-deck)
                 (e/map #(-> [l/->PassDiscard]))
                 emit-game-action)]]]
      <[when (= tab :table)
        <[div {:class "players"} $=
          <[for players $[name]=
            let [hand (get hands name)
                 {:keys [held may-is down]} hand
                 held-count (get hand :held-count (count held))
                 may-ier? (= name may-ier)
                 turn? (= name turn-name)
                 dealer? (= name dealer-name)
                 dealer-class "dealer-chip fas fa-chevron-circle-right"
                 !ui chip
                 (fn [i]
                   <[div {:class {:used (> i may-is)
                                  :active (and may-ier? (= i may-is))
                                  :may-i-chip true}}])]
            <[keyed name
              <[div {:class {:turn turn? :player true}} $=
                <[div {:class "text"} $=
                  <[when dealer?
                    <[i {:class dealer-class}]]
                  <[dom/text name]
                  <[when dealer?
                    <[i {:class [dealer-class "hidden"]}]]]
                <[div {:class "player-contents"} $=
                  <[div {:class "board-hand"} $=
                    <[unknown-cards held-count {:max held-count
                                                :last (and (= name taker) taken)
                                                :delayed? true}]
                    <[div {:class "may-i-chips"} $=
                      <[chip 1]
                      <[chip 2]
                      <[chip 3]]]]]]]]
        <[div {:class "reserved-space"}]]
      ] d-in-game >
    (->> (dom/on-click d-in-game)
         (e/map #(-> nil))
         (dom/emit ::selected))
    let [!ui render-goal
         (fn [type id]
           let [label (type {:set "Set" :run "Run"})]
           <[div {:class "target"} $=
             <[div {:class "target-plays"}]
             <[span {:class {:invalid false}} label]])]
    let [play-board-open? (or (and (not drawn?) first-turn?)
                              (and drawn? turn?))]
    <[when (= tab :table)
      <[div {:style (if play-board-open?
                      {:transform "translateY(0)"}
                      {})
             :class "play-board"} $=
        <[div {:style {:transition "transform 0.5s ease-in-out"
                       :transform (if view-table?
                                    "translateY(100%)"
                                    "translateY(0)")}} $=
          <[when play-board-open?
            <[keyed "play-board-top"
              <[div {:style {:transform "translateY(0)"
                             :delayed {:transform "translateY(-100%)"}
                             :remove {:transform "translateY(0)"}}
                     :class "play-board-top"} $=
                <[when (not drawn?)
                  <[div {:class "round-header"} round-title]]
                <[div {:class {:play-controls true :hidden (not drawn?)}} $=
                  <[button {:disabled (empty? in-play)
                            :class "button is-danger is-small"}
                    "Cancel"] d-cancel >
                  (->> (dom/on-click d-cancel)
                       (e/map #(-> {}))
                       (dom/emit ::plays))
                  <[div {:class "discard-space"} $=
                    <[div {:class "pcard"} $=
                      <[img {:src (-> plays :discard c/from-int c/to-src)}]
                      ] d-discard >
                    (->> (dom/on-click d-discard)
                         (e/map #(merge {:discard selected-card}))
                         (dom/emit ::plays))
                    <[span "Discard"]]
                  <[button {:disabled (not passes?)
                            :class "button is-danger is-small"}
                    "End Turn"] d-end-turn >
                  (->> (dom/on-click d-end-turn)
                       (e/map #(-> [l/->Play plays]))
                       emit-game-action)]
                <[when (not down?)
                  <[div {:class "play-closer is-size-7"} $=
                    <[div]
                    <[div (str "View " (if view-table? "Plays" "Table"))
                      ] d-change-view >
                    (->> (dom/on-click d-change-view)
                         (e/map #(not view-table?))
                         (dom/emit ::view-table?))
                    <[div]]]]]]
          <[div {:class "play-board-body"} $=
            <[when (not down?)
              <[for (range (:set goals)) $[id]=
                <[keyed [:set id]
                  <[render-goal :set id]]]
              <[for (range (:run goals)) $[id]=
                <[keyed [:run id]
                  <[render-goal :run id]]]]]]
        <[div {:class "play-board-footer"} $=
          <[render-hand "hidden-parent"]]]]
    <[when (= tab :table)
      <[div_ {:style {:position "fixed"
                      :width "100%"
                      :z-index "4"
                      :left "0"
                      :bottom "0"}} $=
        <[render-hand "card-parent"]] d-my-hand >
      let [e-nearest
           (->> (dom/on-click d-my-hand)
                (e/remove #(nil? selected-card))
                (e/map (fn [dom-event]
                         (let [client-x (.-clientX dom-event)
                               client-y (.-clientY dom-event)]
                           (->> (js/document.getElementById "card-parent")
                                .-children
                                js/Array.from
                                vec
                                (map #(let [rect (.getBoundingClientRect %1)
                                            dist-x (+ (/ (.-width rect) 2)
                                                      (.-x rect)
                                                      (* -1
                                                         client-x))
                                            dist-y (+ (/ (.-height rect) 2)
                                                      (.-y rect)
                                                      (* -1
                                                         client-y))]
                                        {:card (-> (.. %1 -dataset -cardVal)
                                                   (js/parseInt 10))
                                         :orientation (if (> dist-x 0)
                                                        :left
                                                        :right)
                                         :dist-x (js/Math.abs dist-x)
                                         :dist-y (js/Math.abs dist-y)}))
                                (apply min-key (fn [{:keys [dist-x dist-y]}]
                                                 (+ (* dist-x dist-x)
                                                    (* dist-y dist-y)))))))))
           close? (fn [{:keys [dist-x dist-y]}]
                    (and (< dist-x 46)
                         (< dist-y 35)))]
      (->> e-nearest
           (e/filter close?)
           (e/remove #(= selected-card (:card %)))
           (e/map #(let [{:keys [card orientation]} %
                         left? (= orientation :left)
                         partitioned (->> held
                                          (remove (partial = selected-card))
                                          (partition-by (partial = card)))
                         first? (= (list card) (first partitioned))
                         [pre target post] (concat (if first? [[]] [])
                                                   partitioned
                                                   [[]])]
                     (-> (concat pre
                                 (if left? [selected-card] [])
                                 target
                                 (if left? [] [selected-card])
                                 post)
                         vec)))
           (dom/emit ::held))
      (->> e-nearest
           (e/remove #(and (close? %1) (not= selected-card (:card %1))))
           (e/map #(-> nil))
           (dom/emit ::selected))]])

(defui config-screen []
  state <- (dom/envs :state)
  get-href <- (dom/envs :get-href)
  let [{:keys [room-id id-by-name config]} state
       {:keys [num-decks]} config]
  <[div {:class "card waiting-room"} $=
    <[div {:class "card-header"} $=
      <[div {:class "tags has-addons are-medium"} $=
        <[span {:class "tag is-success"} "Room ID"]
        <[span {:class "tag is-info has-text-weight-bold"} room-id]
        <[span {:class "tag l0nk is-success"} "Copy"] d-copy-id >
        (dom/consume! (dom/on-click d-copy-id) #(copy room-id))]
      <[a {:class "is-small"} "Copy invite link"] d-copy-link >
      (dom/consume! (dom/on-click d-copy-link) #(copy (get-href)))]
    <[div {:class "card-content"} $=
      <[div {:class "content"} $=
        <[table {:class "table is-bordered"} $=
          <[thead $= <[tr $= <[th "Players In Game"]]]
          <[tbody $=
            <[for (keys id-by-name) $[name]=
              <[keyed name <[tr $= <[td name]]]]]]
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
  s-game-states <- (s/reduce (fn [{:keys [curr]} next]
                               {:curr next :prev curr})
                             {}
                             e-game-state)
  s-game-state <- (s/map :curr s-game-states)
  <[dom/assoc-env :s-game-state s-game-state $=
    <[dom/bind s-game-states $[{:keys [curr prev]}]=
      [(js/console.log curr)]
      <[dom/assoc-env :prev prev $=
        <[dom/assoc-env :state curr $=
          <[when (:room-id curr)
            [(set-hash (:room-id curr))]]
          <[div {:class "container content"} $=
            <[case (get-screen curr)
              <[:home <[home-screen]]
              <[:play <[play-screen]]
              <[:config <[config-screen]]]]]]]])
