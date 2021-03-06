(ns ui.entry
  (:require ["clipboard-copy" :as copy]
            [allpa.core :as a]
            [mayu.macros :refer [defui ui]]
            [mayu.frp.event :as e]
            [mayu.frp.signal :as s]
            [mayu.dom :as dom]
            [wayra.core :as w :refer [exec <#>]]
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
    (nil? (:room-id state)) :home
    (:started? state) :play
    :else :config))

(defui card-loop [i]
  <[if (= 0 i)
    <[then <[div {:style {:visibility "hidden"}} "END"]]
    <[else
      <[div {:class "card-loop"} $=
        <[img {:src "/cards/15.0.png"}]
        <[card-loop (dec i)]]]])

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
          <[card-loop 16]]
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

(defn exec-m-f [state m]
  (let [adjusted-state
        (-> state
            (update :hands
                    #(a/map-values (fn [{:keys [held-count] :as hand}]
                                     (if-not (nil? held-count)
                                       (assoc hand :held (range held-count))
                                       hand))
                                   %1)))]
    (->> m
         (exec {:init-state adjusted-state
                :reader {:name (:name state)
                         :cards-per-deck 54}}))))

(defn run-m-f [state m]
  (->> (->> (exec-m-f state m) :result)))

(defn passes?-m-f [state m]
  (->> (exec-m-f state m) :error nil?))

(defn run-m [m]
  (<#> (dom/envs :state)
       #(->> (exec-m-f % m) :result)))

(defn passes?-m [m]
  (<#> (dom/envs :state)
       #(passes?-m-f % m)))

(defui turn-reset [k init]
  let [to-details #(-> [(:turn %1) (:hand %1)])]
  <[dom/memo true $=
    s-game-states <- (dom/envs :s-game-states)
    (->> (s/changed s-game-states)
         (e/filter #(not= (-> %1 :prev to-details)
                          (-> %2 :curr to-details)))
         (e/map #(-> init))
         (dom/emit k))])

(defn ammend-client-held [srvr held]
  (let [to-m #(->> %
                   (a/index-by identity)
                   (a/map-values (fn [] true)))
        m-held (to-m held)
        m-srvr (to-m srvr)]
    (-> (concat (filter #(get m-srvr %) held)
                (remove #(get m-held %) srvr))
        vec)))

(defui play-state [inner]
  state <- (dom/envs :state)
  <[dom/memo (:room-id state) $=
    get-item <- (dom/envs :get-item)
    set-item <- (dom/envs :set-item)
    let [car dom/collect-and-reduce
         {:keys [hands name room-id]} state
         {:keys [may-is held down]} (get hands name)
         storage-key (str (hash [name room-id]))
         storage-held (get-item storage-key)
         init-held (ammend-client-held held storage-held)]
    <[car ::held #(-> %2) init-held $=
      <[car ::tab #(-> %2) :table $=
        <[car ::view-table? #(-> %2) false $=
          (turn-reset ::view-table? false)
          <[car ::plays #(-> %2) {} $=
            (turn-reset ::plays {})
            <[car ::selected #(-> %2) nil $=
              s-game-states <- (dom/envs :s-game-states)
              s-held <- (dom/envs ::held)
              s-tab <- (dom/envs ::tab)
              s-view-table <- (dom/envs ::view-table?)
              s-plays <- (dom/envs ::plays)
              s-selected <- (dom/envs ::selected)
              s-deps <- (s/zip-with #(-> [%1 %2 %3 %4 %5 %6])
                                    s-held
                                    s-tab
                                    s-view-table
                                    s-plays
                                    s-selected
                                    s-game-states)
              s-selected-deps <- (s/zip-with #(hash [%1 %2 %3])
                                             s-held
                                             s-tab
                                             s-plays)
              (->> (s/changed s-selected-deps)
                     (e/map #(-> nil))
                     (dom/emit ::selected))
              <[dom/bind s-deps $[[client-held
                                   picked-tab
                                   view-table?
                                   plays
                                   selected-card
                                   {:keys [curr prev]}]]=
                let [t1 (.now js/Date)]
                <[dom/assoc-env :prev prev $=
                  <[dom/assoc-env :state curr $=
                    let [{:keys [hands name]} curr
                         {:keys [held]} (get hands name)]
                    <[dom/memo [held client-held] $=
                      [(ammend-client-held held client-held)]
                      ] now-held >
                    [(set-item storage-key now-held)]
                    <[inner
                      now-held
                      plays
                      picked-tab
                      selected-card
                      view-table?]
                    let [t2 (.now js/Date)]
                    [(println (str "Render Time " (/ (- t2 t1) 1000) "s"))]]]]]]]]]])

(defui unknown-cards [num-cards & args]
  let [opts (nth args 0 {})
       max-cards (get opts :max 11)
       min-cards (get opts :min 0)
       last (:last opts)
       delayed? (:delayed? opts)
       removed? (:removed? opts)
       spread-count (js/Math.min num-cards max-cards)
       width (+ (* 2 spread-count) 25)
       downed-y #(str "translate(" (* %1 2) "px, " (+ %1 200)"%)")
       steady-y #(str "translate(" (* % 2) "px, 0)")]
  <[dom/memo [num-cards opts] $=
    <[div {:style {:width (str width "px")} :class "deck-top"} $=
      <[for (range (inc (max min-cards num-cards))) $[i]=
        <[keyed i
          let [x (max 0 (- i (- num-cards spread-count)))
               get-styles
               #(merge {:transform (if delayed?
                                     (downed-y %1)
                                     (steady-y %1))}
                       (if delayed?
                         {:delayed {:transform (steady-y %1)}}
                         {})
                       (if removed?
                         {:remove {:transform (downed-y %1)}}
                         {}))]
          <[cond
            <[(< i num-cards)
              <[div {:style (get-styles x)
                     :class "t4 pcard deck-loop"} $=
                <[img {:src "/cards/15.0.png"}]]]
            <[(and last (= i num-cards))
              <[div {:style (-> (get-styles (dec x))
                                (assoc :opacity "1")
                                (assoc-in [:delayed :opacity] "0"))
                     :class "t4 pcard deck-loop"} $=
                <[img {:src (-> last c/from-int c/to-src)}]]]
            <[(< i min-cards)
              <[div {:style (get-styles 0)
                     :class "t4 pcard deck-loop"} $=
                <[img {:src (-> nil c/from-int c/to-src)}]]]
            <[:else []]]]]
      ] d-stack >
    <[div {:class "full-count"} num-cards]
    [d-stack]])

(defui play-screen []
  <[play-state $[held plays picked-tab selected-card view-table?]=
    s-plays <- (dom/envs ::plays)
    s-selected <- (dom/envs ::selected)
    prev <- (dom/envs :prev)
    state <- (dom/envs :state)
    hand-winner <- (run-m l/get-hand-winner)
    request-states <- (run-m l/get-request-states)
    let [prev-discard (:discard prev)
         prev-hands (:hands prev)
         {:keys [paused?
                 discard
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
         dealer? (= name dealer-name)
         my-turn? turn?
         id (get name-by-id name)
         hand (get hands name)
         {:keys [may-is down]} hand
         can-may-i-pre-pause? (and (not drawn?)
                                   (not turn?)
                                   (> may-is 0))
         can-may-i? (and can-may-i-pre-pause? (not paused?))
         passing? (= false (get discard-requests name))
         may-ing? (= true (get discard-requests name))
         may-ier (and (not drawn?)
                      (->> request-states
                           (drop 1)
                           (filter #(= :request (:request-state %)))
                           (map :name)
                           first))
         picking? (and (not hand-winner) turn? (not drawn?))
         intends? (= false (get discard-requests turn-name))
         down? (not (empty? down))
         awaiting-you? (and (not drawn?)
                            (->> l/get-request-states
                                 (run-m-f (assoc state :discard-requests {}))
                                 (filter #(= name (:name %)))
                                 first
                                 :request-state
                                 (= :na))
                            intends?)
         leader (->> money
                     (a/map-values vector)
                     vals
                     (apply max-key first)
                     last)]
    let [!rec leafs (fn [thing]
                      (cond
                        (map? thing) (->> thing
                                          (a/map-values leafs)
                                          vals
                                          flatten)
                        (coll? thing) (flatten (map leafs thing))
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
           <[dom/memo (if (nil? parent-id)
                        (count held)
                        [held selected-card plays]) $=
             <[div {:class "tn my-hand"
                    :delayed {:class "my-hand"}} $=
               <[div {:id (or parent-id "hidden-parent")
                      :class "card-parent"} $=
                 <[for held $[card]=
                   <[keyed card
                     <[div {:style {:opacity "0"
                                    :transform "translateX(200%)"
                                    :delayed {:opacity "1"
                                              :transform "translateX(0)"}}
                            :class "t1 pcard" :data-card-val card} $=
                       <[img {:class {:clickable (nil? selected-card)
                                      :selected (= card selected-card)
                                      :in-play (get in-play card)}
                              :src (-> card c/from-int c/to-src)}]
                       ] d-hand >
                     (->> (dom/on-click d-hand)
                          (e/filter #(nil? selected-card))
                          (e/map #(-> card))
                          (dom/emit ::selected))]]]]])]
    <[div {:style {:position "relative"}
           :class "in-game"} $=
      <[div {:style {:position "fixed"
                     :z-index "9"}
             :class "tab-bar field has-addons"} $=
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
        <[keyed "main-controls"
          <[div {:style {:padding-top "2em"
                         :width "100%"
                         :transform "translateY(-100%)"
                         :delayed {:transform "translateY(0)"}
                         :remove {:position "absolute"
                                  :transform "translateY(-100%)"}}
                 :class "tn t2 main-controls"
                 :delayed {:class "t2 main-controls"}} $=
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
              <[div {:class "grant"} $=
                <[if (and may-ier turn? (not drawn?) (not intends?))
                  <[then
                    <[button {:class "button is-info is-small"} "Grant May I"
                      ] d-grant >
                    (->> (dom/on-click d-grant)
                         (e/map #(-> [l/->PassDiscard]))
                         emit-game-action)]
                  <[else <[div]]]]
              <[dom/memo discard $=
                <[div {:style {:position "relative"}
                       :class "pcard discarded"} $=
                  <[img {:src (-> nil c/from-int c/to-src)}]
                  <[for (reverse discard) $[card]=
                    <[keyed card
                      <[div {:style {:position "absolute"
                                     :left "0"
                                     :top "0"
                                     :margin "0"
                                     :transform "translateY(200%)"
                                     :opacity "0"
                                     :delayed {:transform "translateY(0)"
                                               :opacity "1"}
                                     :remove {:transform "translateY(200%)"
                                              :opacity "0"}}
                             :class "t1 pcard"} $=
                        <[img {:src (-> card c/from-int c/to-src)}]]]]
                  ]] d-discard >
              <[unknown-cards deck-count {:removed? true}] d-deck >
              (->> (dom/on-click d-discard)
                   (e/filter #(and (not drawn?) turn?))
                   (e/map #(-> [l/->RequestDiscard]))
                   emit-game-action)
              (->> (dom/on-click d-deck)
                   (e/filter #(and (not drawn?) turn?))
                   (e/map #(if passing? [l/->Cancel] [l/->PassDiscard]))
                   emit-game-action)]]]]
      <[when (= tab :scores)
        <[keyed "scores"
          <[div {:class "score-container"} $=
            <[table {:class "table"} $=
              <[thead $=
                <[tr $=
                  <[th "Hand"]
                  <[for players $[name]=
                    <[keyed name
                      <[th $=
                        <[dom/text name]
                        <[when (and (= leader name) game-over?)
                          <[i {:class "fas fa-crown"}]]]]]]]
              <[tbody $=
                <[for (range 7) $[hand]=
                  <[keyed hand
                    <[tr $=
                      <[th (inc hand)]
                      <[for players $[name]=
                        <[keyed name
                          <[td (get-in scores [name hand])]]]]]]
                <[tr $=
                  <[th "Total"]
                  <[for players $[name]=
                    <[keyed name
                      <[td (reduce + 0 (get scores name))]]]]]
              <[tfoot $=
                <[tr $=
                  <[th "Cash"]
                  <[for players $[name]=
                    let [cash (get money name)
                         p (cond
                             (= 0 cash) ""
                             (> cash 0) "+"
                             :else "-")
                         i (js/Math.floor (/ (js/Math.abs cash) 100))
                         d- (mod (js/Math.abs cash) 100)
                         d (str (if (< d- 10) "0" "") d-)]
                    <[keyed name
                      <[td (str "$" p i "." d)]]]]]]]]]
      <[when (= tab :table)
        <[div {:class "tn players"
               :delayed {:class "players"}} $=
          <[when (or hand-winner (and intends? (not drawn?)))
            <[div {:class "t3"
                   :style {:position "fixed"
                           :top "0"
                           :left "0"
                           :display "flex"
                           :justify-content "center"
                           :height "100vh"
                           :width "100vw"
                           :background "rgba(0, 0, 0, 0.5)"
                           :flex-direction "column"
                           :align-items "stretch"
                           :z-index "2"
                           :opacity "0"
                           :delayed {:opacity "1"}
                           :remove {:opacity "0"}}} $=
              <[div {:style {:text-align "center"
                                :padding "10px 1.5em"
                                :background "black"}} $=
                <[div {:style {:max-width "960px" :margin "auto"}} $=
                  <[when hand-winner
                    <[p {:class "is-size-5"
                         :style {:color "white"}} $=
                      <[dom/text "Hand won by "]
                      <[span {:class "has-text-weight-bold"} hand-winner]
                      <[dom/text "!"]]
                    <[when dealer?
                      <[button {:class "button is-danger"} "Deal Next Hand"
                        ] d-deal >
                      (->> (dom/on-click d-deal)
                           (e/map #(-> [l/->Deal]))
                           emit-game-action)]]
                  <[when intends?
                    <[div {:class "t3"
                           :style {:opacity (if paused? "0.5" "1")}} $=
                      <[if turn?
                        <[then
                          <[div (str "You have indicated you would like to"
                                     " draw from the deck...")]
                          <[button {:disabled (boolean paused?)
                                    :style {:margin "10px 0"}
                                    :class "button is-small is-danger"} "Cancel"
                            ] d-cancel >
                          (->> (dom/on-click d-cancel)
                               (e/map #(-> [l/->Cancel]))
                               emit-game-action)]
                        <[else
                          <[div $=
                            <[span {:class "has-text-weight-bold"} turn-name]
                            <[dom/text (str " has indicated they would like to"
                                            " draw from the deck. "
                                            (cond
                                              (not can-may-i-pre-pause?) ""
                                              awaiting-you?
                                              "What would you like to do?"
                                              :else
                                              (str "We assume you are passing so "
                                                   "you don't need to input "
                                                   "anything but you can still "
                                                   "May I if you like.")))]]
                          <[when can-may-i-pre-pause?
                            <[div {:style {:margin "10px 0 5px 0"}
                                   :class "field has-addons"} $=
                              <[p {:class "control" :style {:flex "1"}} $=
                                <[button {:class "button is-static"
                                          :style {:visibility "hidden"}}]]
                              <[when awaiting-you?
                                <[p {:class "control"} $=
                                  <[button {:disabled (boolean paused?)
                                            :class {:button true
                                                    :is-warning (not passing?)
                                                    :is-info passing?
                                                    :is-small true}}
                                    (if passing? "Cancel" "Pass")
                                    ] d-pass >
                                  (->> (dom/on-click d-pass)
                                       (e/map #(if passing?
                                                 [l/->Cancel]
                                                 [l/->PassDiscard]))
                                       emit-game-action)]
                                <[p {:class "control" :style {:flex "0.5"}} $=
                                  <[button {:class "button is-static"
                                            :style {:visibility "hidden"}}]]
                                <[p {:class "control"
                                     :style {:min-width "40px" :max-width "40px"}} $=
                                  <[button {:class "button is-static"
                                            :style {:visibility "hidden"}}]]
                                <[p {:class "control" :style {:flex "0.5"}} $=
                                  <[button {:class "button is-static"
                                            :style {:visibility "hidden"}}]]]
                              <[p {:class "control"} $=
                                <[button {:disabled (boolean paused?)
                                          :class {:button true
                                                  :is-danger (not may-ing?)
                                                  :is-info may-ing?
                                                  :is-small true}}
                                  (if may-ing? "Cancel" "May I")
                                  ] d-may-i >
                                (->> (dom/on-click d-may-i)
                                     (e/map #(if may-ing?
                                               [l/->Cancel]
                                               [l/->RequestDiscard]))
                                     emit-game-action)]
                              <[p {:class "control" :style {:flex "1"}} $=
                                <[button {:class "button is-static"
                                          :style {:visibility "hidden"}}]]]]]]]
                    let [awaiting
                         (->> request-states
                              (drop 1))]
                    <[div {:style {:margin-top "1.5em"
                                   :display "flex"
                                   :flex-direction "column"
                                   :align-items "stretch"}} $=
                      <[div $=
                        <[dom/text
                          (cond
                            (not paused?) ""
                            (nil? may-ier) "No May Is. Giving top of deck to "
                            :else "May I given to ")]
                        <[span {:class "has-text-weight-bold"}
                          (cond
                            (not paused?) "Waiting On"
                            (nil? may-ier) turn-name
                            :else may-ier)]]
                      <[for awaiting $[{:keys [name request-state]}]=
                        <[keyed name
                          let [didnt? (and paused? (= request-state :na))]
                          <[div {:class {:request-state true
                                         :rs-pass (or (= request-state :pass)
                                                      didnt?)
                                         :rs-auto (or (not= (get discard-requests
                                                                 name)
                                                            false)
                                                      didnt?)
                                         :rs-may-i (and paused? (= name may-ier))
                                         :rs-request (= request-state :request)}}
                            name]]]
                      <[div {:style {:align-self "flex-end"}
                             :class "is-size-7"} $=
                        <[span "( "]
                        <[span {:class "request-state"} "Waiting"]
                        <[span ", "]
                        <[span {:class "request-state rs-pass rs-auto"}
                          "Auto-pass"]
                        <[span ", "]
                        <[span {:class "request-state rs-pass"}
                          "Pass"]
                        <[span ", "]
                        <[span {:class "request-state rs-request"}
                          "May I"]
                        <[span ", "]
                        <[span {:class "request-state rs-request rs-may-i"}
                          "May I Granted"]
                        <[span " )"]]]]]]]]
          <[dom/memo [hands (:table plays) turn-name dealer-name drawn?] $=
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
                                                  :min 1
                                                  :last (and (= name taker) taken)
                                                  :delayed? true}]
                      <[div {:class "may-i-chips"} $=
                        <[chip 1]
                        <[chip 2]
                        <[chip 3]]]]
                  <[for (keys down) $[type]=
                    let [req (type goals)
                         piles (get-in plays [:table name type] {})]
                    <[for (map vector (range) (type down)) $[[id pile]]=
                      let [[pre post] (get piles id)
                           guid (hash [type id name])
                           all (vec (concat pre pile post))
                           all-count (count all)]
                      <[keyed guid
                        let [spread-width (+ 35 (* 15 all-count))]
                        <[div {:id guid
                               :class "extra-cards"
                               :style {:width (str spread-width "px")}} $=
                          <[for (map vector (range) all) $[[i card]]=
                            let [transform #(str "translate("
                                                 (* i 15)
                                                 "px, " %
                                                 ")")]
                            <[keyed card
                              <[div {:class "t2 pcard"
                                     :style {:transform
                                             (transform "200%")
                                             :delayed
                                             {:transform (transform "0")}}} $=
                                <[img {:class {:selected (get in-play card)}
                                       :src (-> card c/from-int c/to-src)}]]]]
                          ] d-pile >
                        (->> (dom/on-click {:capture true} d-pile)
                             (e/filter #(and (s/inst! s-selected) my-turn? drawn?))
                             (e/map #(let [selected-card (s/inst! s-selected)
                                           client-x (.-clientX %)
                                           el (js/document.getElementById guid)
                                           rect (.getBoundingClientRect el)
                                           x (.-x rect)
                                           width (.-width rect)
                                           orientation (if (> (* 2
                                                                 (- client-x
                                                                    x))
                                                              width)
                                                         :right
                                                         :left)
                                           left? (= orientation :left)
                                           [new-pre new-post]
                                           [(if left?
                                              (vec (concat [selected-card] pre))
                                              pre)
                                            (if left?
                                              post
                                              (vec (concat post [selected-card])))]]
                                       {:pre new-pre :post new-post :pile pile}))
                             (e/filter (fn [{:keys [pre pile post]}]
                                         (->> (concat pre pile post)
                                              vec
                                              (l/validate-play type)
                                              (passes?-m-f state))))
                             (e/map #(-> [(:pre %1) (:post %1)]))
                             (e/map #(->> %
                                          (assoc piles id)
                                          (assoc-in (s/inst! s-plays)
                                                    [:table name type])))
                             (dom/emit ::plays))]]]]]]]
          <[div {:class "reserved-space"}]]]]
    d-in-game >
    (->> (dom/on-click {:capture false} d-in-game)
         (e/map #(-> nil))
         (dom/emit ::selected))
    let [!ui render-goal
         (fn [type id]
           let [req (type {:set 3 :run 4})
                curr-type (get-in plays [:down type]
                                  (->> (range (type goals))
                                       (map #(->> (range req)
                                                  (map (fn [] nil))
                                                  vec))
                                       vec))
                curr (nth curr-type id)
                spread (drop (dec req) curr)
                spread-width (+ 15 (* 15 (count spread)))
                label (type {:set "Set" :run "Run"})]
           <[dom/memo [type id curr] $=
             <[div {:class "target"} $=
               <[div {:class "target-plays"} $=
                 <[for (range req) $[n]=
                   <[keyed n
                     let [last? (= n (dec req))
                          full? (every? (comp not nil?) curr)
                          [pre post] (if (and last? full?)
                                       [curr []]
                                       (split-at n curr))
                          back? (every? (comp not nil?) post)
                          [non-nil nil-then] (split-with (comp not nil?)
                                                         (if back? pre post))
                          dropped (concat non-nil (drop 1 nil-then))
                          [pre post] (if back? [dropped post] [pre dropped])]
                     <[if last?
                       <[then
                         <[div {:style {:width (str spread-width "px")
                                        :position "relative"
                                        :display "inline-block"
                                        :margin "0 5px"
                                        :height "45px"}} $=
                           <[for (map vector (range) spread) $[[i card]]=
                             <[div {:class "pcard"
                                    :style {:position "absolute"
                                            :margin "0"
                                            :top "0"
                                            :left "0"
                                            :transform (str "translateX("
                                                            (* i 15)
                                                            "px)")}} $=
                               <[img {:src (-> card c/from-int c/to-src)}]]]]]
                       <[else
                         <[div {:class "pcard"} $=
                           <[img {:src (-> curr (nth n) c/from-int c/to-src)}]
                           ]]] d-target >
                     (->> (dom/on-click d-target)
                          (e/map #(let [curr-type (get-in (s/inst! s-plays)
                                                          [:down type]
                                                          (->> (range (type goals))
                                                               (map (fn []
                                                                      (->> (range req)
                                                                           (map (fn [] nil))
                                                                           vec)))
                                                               vec))
                                        selected-card (s/inst! s-selected)
                                        added (if back?
                                                (concat pre post [selected-card])
                                                (concat pre [selected-card] post))
                                        [reqd extra] (split-at req added)
                                        extra-used (->> extra
                                                        reverse
                                                        (remove nil?)
                                                        reverse)
                                        updated (vec (concat reqd extra-used))]
                                    (->> updated
                                         (assoc curr-type id)
                                         (assoc-in (s/inst! s-plays)
                                                   [:down type]))))
                          (dom/emit ::plays))]]]
               let [any? (->> curr (remove nil?) empty? not)]
               invalid? <- <[when any?
                             (<#> (passes?-m (l/validate-play type curr))
                                  not)]
               <[span {:class {:invalid invalid?}} label]
               <[when any?
                 <[span {:class "cancel"} "✗"] d-cancel >
                 (->> (dom/on-click d-cancel)
                      (e/map #(-> (s/inst! s-plays)
                                  (assoc-in [:down type id] (->> (range req)
                                                                 (map (fn [] nil))
                                                                 vec))
                                  (update :down (fn [downs]
                                                  (if (every? nil? (leafs downs))
                                                    nil
                                                    downs)))))
                      (dom/emit ::plays))]]])]
    let [play-board-open? (or (and (not drawn?) first-turn?)
                              (and drawn? turn?))]
    <[dom/stash $=
      <[div {:class "play-board-top"} $=
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
                 (e/map #(merge (s/inst! s-plays) {:discard selected-card}))
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
            <[div]]]]
      ] top-stash >
    let [!ui render-top (fn [] <[dom/unstash top-stash])]
    <[when (= tab :table)
      <[when play-board-open?
        <[keyed [tab "play-board"]
          <[div {:style {:transform "translateY(100%)"
                         :delayed {:transform "translateY(0)"}
                         :remove {:transform "translateY(100%)"}}
                 :class "play-board"} $=
            <[div {:style {:visibility "hidden"}} $=
              <[render-top]]
            <[div {:class "t2 play-board-body-wrapper"
                   :style {:transform (if view-table?
                                        "translateY(100%)"
                                        "translateY(0)")}} $=
              <[div {:style {:position "absolute"
                             :left "0"
                             :top "0"
                             :z-index "2"
                             :width "100%"
                             :transform "translateY(-100%)"}} $=
                <[render-top]]
              <[div {:class "play-board-body"} $=
                <[when (not down?)
                  <[for (range (:set goals)) $[id]=
                    <[keyed [:set id]
                      <[render-goal :set id]]]
                  <[for (range (:run goals)) $[id]=
                    <[keyed [:run id]
                      <[render-goal :run id]]]]]
              <[div {:class "play-board-footer"} $=
                <[render-hand nil]]]
            ] d-play-board >
          (->> (dom/on-click {:capture false} d-play-board)
               (e/map #(-> nil))
               (dom/emit ::selected))]]]
    <[when (= tab :table)
      <[div_ {:class "t2"
              :style {:position "fixed"
                      :width "100%"
                      :z-index "4"
                      :left "0"
                      :bottom "0"
                      :transform "translateY(100%)"
                      :delayed {:transform (str "translateY("
                                                (if view-table? "100%" "0")
                                                ")")}
                      :remove {:transform "translateY(100%)"}}} $=
        <[render-hand "card-parent"]] d-my-hand >
      let [e-nearest
           (->> (dom/on-click d-my-hand)
                (e/remove #(nil? selected-card))
                (e/map (fn [dom-event]
                         (let [client-x (.-clientX dom-event)
                               client-y (.-clientY dom-event)]
                           [(->> (js/document.getElementById "card-parent")
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
                                         {:card (-> (.. ^js %1 -dataset -cardVal)
                                                    (js/parseInt 10))
                                          :orientation (if (> dist-x 0)
                                                         :left
                                                         :right)
                                          :dist-x (js/Math.abs dist-x)
                                          :dist-y (js/Math.abs dist-y)}))
                                 (apply min-key (fn [{:keys [dist-x dist-y]}]
                                                  (+ (* dist-x dist-x)
                                                     (* dist-y dist-y)))))
                            dom-event]))))
           close? (fn [[{:keys [dist-x dist-y]}]]
                    (and (< dist-x 46)
                         (< dist-y 35)))
           stop-propagation (partial e/map (fn [[nearest dom-event]]
                                             (.stopPropagation dom-event)
                                             nearest))]
      (->> e-nearest
           (e/filter close?)
           (e/remove #(= selected-card (:card (first %))))
           stop-propagation
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
           (e/remove #(and (close? %1) (not= selected-card (:card (first %1)))))
           stop-propagation
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
  let [e-error-defer (e/defer e-error 5000)
       e-auto-dismiss (->> (e/tag-with vector e-error-defer s-error)
                           (e/filter #(apply = %1))
                           (e/map #(-> nil)))]
  <[dom/collect ::error-display $[e-error-display]=
    (dom/emit ::error-display e-error)
    (dom/emit ::error-display e-auto-dismiss)
    s-error-display <- (s/from nil e-error-display)
    <[dom/bind s-error-display $[error-display]=
      <[when error-display
        <[div {:class "t3 notification is-danger"
               :style {:position "fixed"
                       :top "1em"
                       :left "calc(50% - 10em)"
                       :width "20em"
                       :z-index "10"
                       :opacity "0"
                       :delayed {:opacity "1"}
                       :remove {:opacity "0"}}} $=
          <[button {:class "delete"} ] d-dismiss >
          (->> (dom/on-click d-dismiss)
               (e/map #(-> nil))
               (dom/emit ::error-display))
          <[dom/text error-display]]]]]
  s-game-states <- (s/reduce (fn [{:keys [curr prev]} next]
                               (if (= next curr)
                                 {:curr curr :prev prev}
                                 {:curr next :prev curr}))
                             {}
                             e-game-state)
  s-defered <- (s/defer s-game-states 2500)
  s-all <- (s/zip-with (fn [fast slow]
                         (let [{:keys [drawn?
                                       turn
                                       name-by-id
                                       discard-requests]} (:curr fast)
                               request-states (run-m-f (assoc (:prev fast)
                                                              :discard-requests
                                                              {})
                                                       l/get-request-states)
                               base (if (and drawn?
                                             (-> discard-requests
                                                 (get (get name-by-id turn))
                                                 (= false))
                                             (not (nil? (:curr slow)))
                                             (not= (:curr slow) (:curr fast))
                                             (->> request-states
                                                  (drop 1)
                                                  (map :request-state)
                                                  (every? #(= :pass %))
                                                  not))
                                      (assoc (:prev fast) :paused? true)
                                      (:curr fast))]
                           {:curr (assoc base :discard-requests discard-requests)
                            :prev (:prev fast)}))
                       s-game-states
                       s-defered)
  s-game-state <- (s/map :curr s-all)
  <[dom/assoc-env :s-game-state s-game-state $=
    <[dom/assoc-env :s-game-states s-all $=
      <[dom/bind s-all $[{:keys [curr prev]}]=
        <[dom/assoc-env :prev prev $=
          <[dom/assoc-env :state curr $=
            <[when (:room-id curr)
              [(set-hash (:room-id curr))]
              <[dom/memo [(:name curr) (:room-id curr)] $=
                (->> (e/timer 2000)
                     (e/map #(-> [l/->Ping (:name curr)]))
                     emit-game-action)]]
            <[keyed "main-content"
              <[div {:class "container content"} $=
                <[case (get-screen curr)
                  <[:home <[home-screen]]
                  <[:play <[play-screen]]
                  <[:config <[config-screen]]]]]]]]]])
