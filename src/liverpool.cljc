(ns liverpool
  (:require [card :as c]
            [allpa.core :as a
             :refer [deftagged defprotomethod]]
            [wayra.core :as w
             :refer [mdo defm defnm fnm pure <#>]]))

(comment :state
         {:discard []
          :config {:num-decks 2}
          :deck []
          :first-turn? false
          :started? false
          :drawn? false
          :id-by-name {"mitch" 0
                       "mikey" 1}
          :name-by-id {0 "mitch"
                       1 "mikey"}
          :money {}
          :scores {}
          :hand 0
          :dealer 0
          :turn 0
          :hands {"name" {:held []
                          :down {:set [[0 13 26]
                                       [1 14 27]]
                                 :run [[2 3 4 5]]}
                          :may-is 0}}
          :discard-requests {}})

(def hand-goals
  [{:set 2 :run 0}
   {:set 1 :run 1}
   {:set 0 :run 2}
   {:set 3 :run 0}
   {:set 2 :run 1}
   {:set 1 :run 2}
   {:set 0:run 3}])

;; Requests
(deftagged CreateRoom [name])
(deftagged JoinRoom [room-id name])
(deftagged ConfigureRoom [room-id config])
(deftagged StartGame [room-id])
(deftagged PassDiscard [room-id])
(deftagged RequestDiscard [room-id])
(deftagged Cancel [room-id])
(deftagged Play [room-id plays])
(deftagged Deal [room-id])
(deftagged Ping [room-id name])

(defn valid-run-order? [cards]
  (->> cards
       (reduce (fn [{:keys [valid? last-rank]} card]
                 (cond
                   (or (not valid?) (= last-rank 13)) {:valid? false}
                   (c/joker? card) {:valid? true
                                    :last-rank (if (nil? last-rank)
                                                 nil
                                                 (inc last-rank))}
                   (nil? last-rank) {:valid? true :last-rank (:n card)}
                   :else {:valid? (= (mod (inc last-rank) 13) (:n card))
                          :last-rank (inc last-rank)}))
               {:valid? true :last-rank nil})
       :valid?))

;; type | :set :run
;; play | [Int]
(defnm validate-play [type play]
  {:keys [cards-per-deck name]} <- w/ask
  state <- w/get
  let [{:keys [config hands]} state
       {:keys [num-decks]} config
       max-card (dec (* num-decks cards-per-deck))
       held (get-in hands [name :held])
       held-map (->> held
                     (a/index-by identity)
                     (a/map-values #(-> true)))]
  (w/eachm play
           (fnm [i]
                (w/whenm (= (c/->NoCard) (c/from-int i))
                         (w/fail "Play contained non-card"))
                (w/whenm (> i max-card)
                         (w/fail "Play contained card outside of decks"))))
  (w/whenm (not= (count play) (count (a/index-by identity play)))
           (w/fail "Play contained duplicate cards"))
  let [cards (map c/from-int play)]
  (w/whenm (every? c/joker? cards)
           (w/fail "Play cannot contain all jokers"))
  (case type
    :set (mdo
          (w/whenm (< (count cards) 3)
                   (w/fail "Set play did not contain at least 3 cards"))
          (w/whenm (->> cards
                        (remove c/joker?)
                        (group-by :n)
                        count
                        (not= 1))
                   (w/fail "Set play contained multiple ranks")))
    :run (mdo
          (w/whenm (< (count cards) 4)
                   (w/fail "Run play did not contain at least 4 cards"))
          (w/whenm (->> cards
                        (remove c/joker?)
                        (group-by :suit)
                        count
                        (not= 1))
                   (w/fail "Run play contained multiple suits"))
          (w/whenm (not (or (valid-run-order? cards)
                            (valid-run-order? (reverse cards))))
                   (w/fail "Run play did not contain ordered ranks")))
    (w/fail "Invalid play type")))

(defm get-hand-winner
  {:keys [hands] :as state} <- w/get
  [(->> hands
        keys
        (filter #(-> hands (get-in [% :held]) empty?))
        first)])

(def get-hand-over? (<#> get-hand-winner boolean))

(defnm add-to-pid [pid n]
  {:keys [id-by-name]} <- w/get
  [(mod (+ pid n)
        (count id-by-name))])

(defnm inc-pid [pid]
  (add-to-pid pid 1))

(defnm dec-pid [pid]
  (add-to-pid pid -1))

(defm ensure-not-drawn
  state <- w/get
  let [{:keys [drawn?]} state]
  (w/whenm drawn?
           (w/fail "Player has already drawn")))

(defm get-request-states
  {:keys [name-by-id
          turn
          discard-requests
          hands
          first-turn?]} <- w/get
  (w/mapm (fnm [pid]
               id <- (add-to-pid pid turn)
               discard-id <- (dec-pid turn)
               let [name (get name-by-id id)
                    requesting? (get discard-requests name)
                    hand (get hands name)
                    turn? (= id turn)
                    discard? (= id discard-id)]
               [{:name name
                 :request-state
                 (cond
                   (and turn? requesting?) :request
                   (and turn? (nil? requesting?)) :na
                   (= (:may-is hand) 0) :pass
                   (= requesting? true) :request
                   (= requesting? false) :pass
                   (and discard? (not first-turn?)) :pass
                   (not (empty? (:down hand))) :pass
                   :else :na)}])
          (range (count name-by-id))))

(defnm progress-when-possible-helper [request-states]
  {:keys [shuffle]} <- w/ask
  {:keys [name-by-id
          turn
          deck
          discard]} <- w/get
  let [turn-name (get name-by-id turn)]
  let [[{:keys [name request-state]}] (drop-while #(= :pass (:request-state %1))
                                                  request-states)]
  let [all-pass? (every? #(= :pass (:request-state %1)) request-states)
       can-request? (= request-state :request)]
  (and (not all-pass?) (not can-request?)) --> []
  (w/whenm all-pass?
           let [[top & rest] deck
                new-deck (if (empty? rest)
                           (shuffle discard)
                           rest)
                new-discard (if (empty? rest)
                              []
                              discard)]
           (w/modify #(-> %1
                          (update-in [:hands turn-name :held]
                                     (a/curry conj top))
                          (assoc :deck (vec new-deck))
                          (assoc :discard (vec new-discard))
                          (assoc :drawn? true)
                          (assoc :discard-requests {}))))
  (w/whenm (and can-request? (= name turn-name))
           let [[top & rest] discard]
           (w/modify #(-> %1
                          (update-in [:hands turn-name :held]
                                     (a/curry conj top))
                          (assoc :discard (vec rest))
                          (assoc :drawn? true)
                          (assoc :discard-requests {}))))
  (w/whenm (and can-request? (not= name turn-name))
           let [needs-reshuffle? (<= (count deck) 3)
                [top & rest-discard] discard
                [d1 d2 d3 & rest-deck] (concat deck
                                               (if needs-reshuffle?
                                                 (shuffle rest-discard)
                                                 []))]
           (w/modify #(-> %1
                          (update-in [:hands turn-name :held]
                                     (a/curry conj d3))
                          (update-in [:hands name :held]
                                     (a/curry conj top d1 d2))
                          (update-in [:hands name :may-is] dec)
                          (assoc :deck (vec rest-deck))
                          (assoc :discard (if needs-reshuffle?
                                            []
                                            (vec rest-discard)))
                          (assoc :drawn? true)
                          (assoc :discard-requests {})))))

(defm progress-when-possible
  request-states <- get-request-states
  (progress-when-possible-helper request-states))

(defm distribute-cards
  {:keys [shuffle cards-per-deck]} <- w/ask
  {:keys [config id-by-name]} <- w/get
  let [{:keys [num-decks]} config]
  let [full-deck (-> (range (* num-decks cards-per-deck))
                     shuffle
                     vec)]
  player-hands <- (w/mapm (fnm [[id name]]
                               init <- (dec-pid id)
                               [{:name name
                                 :down {}
                                 :may-is 3
                                 :held (->> (range 11)
                                            (map (fn [card-id]
                                                   (->> id-by-name
                                                        count
                                                        (* card-id)
                                                        (+ init)
                                                        (nth full-deck))))
                                            vec)}])
                          (vals (a/map-values vector id-by-name)))
  let [num-dealt (* 11 (count id-by-name))
       discard [(nth full-deck num-dealt)]
       deck (vec (drop (inc num-dealt) full-deck))]
  [{:hands (->> player-hands
                (a/index-by :name)
                (a/map-values #(dissoc %1 :name)))
    :discard discard
    :deck deck}])

(defprotomethod handle-game-action [action]
  JoinRoom
  (mdo
   let [{:keys [name]} action]
   {:keys [started?]} <- w/get
   (w/whenm started?
            (w/fail "Game already in progress"))
   (w/modify #(assoc-in %1 [:id-by-name name] 0)))

  ConfigureRoom
  (mdo
   let [{:keys [config]} action]
   {:keys [started?]} <- w/get
   (w/whenm started?
            (w/fail "Game already in progress"))
   (w/modify #(assoc %1 :config config)))

  StartGame
  (mdo
   {:keys [shuffle]} <- w/ask
   state <- w/get
   let [{initial-id-by-name :id-by-name
         :keys [config started?]} state]
   started? --> (w/fail "game already started")
   let [player-names (keys initial-id-by-name)
        ordered-names (shuffle player-names)
        names-with-id (map vector ordered-names (range))
        name-by-id (->> names-with-id
                        (a/index-by last)
                        (a/map-values first))
        id-by-name (->> names-with-id
                        (a/index-by first)
                        (a/map-values last))]
   (w/modify #(assoc %1 :id-by-name id-by-name))
   {:keys [discard deck hands]} <- distribute-cards
   (w/modify (a/curry merge
                      {:name-by-id name-by-id
                       :started? true
                       :first-turn? true
                       :drawn? false
                       :hand 0
                       :dealer 0
                       :turn 1
                       :discard-requests {}
                       :money (a/map-values #(-> 0) id-by-name)
                       :scores (a/map-values #(-> []) id-by-name)
                       :hands hands
                       :discard discard
                       :deck deck})))

  PassDiscard
  (mdo
   ensure-not-drawn
   {:keys [name]} <- w/ask
   (w/modify #(assoc-in %1 [:discard-requests name] false))
   progress-when-possible)

  RequestDiscard
  (mdo
   ensure-not-drawn
   {:keys [name]} <- w/ask
   (w/modify #(assoc-in %1 [:discard-requests name] true))
   progress-when-possible)

  Cancel
  (mdo
   ensure-not-drawn
   {:keys [name]} <- w/ask
   (w/modify #(update %1 :discard-requests (a/curry dissoc name)))
   progress-when-possible)

  Play
  (mdo
   {:keys [name]} <- w/ask
   {:keys [name-by-id
           drawn?
           turn
           hands
           hand
           discard]} <- w/get
   let [hand-id hand
        discard-pile discard
        turn-name (get name-by-id turn)
        {:keys [plays]} action
        {:keys [discard down table]} plays
        hand (get hands name)
        held (:held hand)
        held-map (->> held
                      (a/index-by identity)
                      (a/map-values #(-> true)))
        already-down? (not (empty? (:down hand)))
        discard? (not (c/no-card? (c/from-int discard)))
        down? (not (empty? down))
        table? (not (empty? table))]
   (w/whenm (not= name turn-name)
            (w/fail "Only player whose turn it is can play"))
   (w/whenm (not drawn?)
            (w/fail "Cannot play until done drawing"))
   (w/whenm (and down? already-down?)
            (w/fail "Already down"))
   (w/whenm (and table? (not already-down?) (not down?))
            (w/fail "Cannot play on others until down"))

   (w/modify #(assoc % :played (if discard? [discard] [])))
   (w/eachm down
            (fnm [[type plays]]
                 (w/eachm plays
                          (fnm [play]
                               (validate-play type play)
                               (w/modify #(update % :played (partial concat play)))))))
   let [process-table-play
        (fnm [player type target pre post]
             let [current (get-in hands [player :down type target])
                  updated (concat pre current post)]
             (w/whenm (empty? current)
                      (w/fail "Playing on invalid target"))
             (validate-play type updated)
             (w/modify #(-> %
                            (update :played (partial concat pre post))
                            (assoc-in [:hands player :down type target]
                                      updated))))]
   (w/eachm table
            (fnm [[player plays]]
                 (w/eachm plays
                          (fnm [[type plays]]
                               (w/eachm plays
                                        (fnm [[target [pre post]]]
                                             (process-table-play player
                                                                 type
                                                                 target
                                                                 pre
                                                                 post)))))))
   (w/whenm down?
            let [{:keys [set run]} (nth hand-goals hand-id)]
            (w/whenm (not= set (count (:set down)))
                     (w/fail "Not enough sets played"))
            (w/whenm (not= run (count (:run down)))
                     (w/fail "Not enough runs played")))
   played <- (w/gets :played)
   let [played-set (set played)
        won? (= (count played) (count held))]
   (w/eachm played-set
            (fnm [i]
                 (w/whenm (not (get held-map i))
                          (w/fail "Play contained card not in hand"))))
   (w/whenm (not= (count played-set) (count played))
            (w/fail "Used duplicate cards in play"))
   (w/whenm (or (and (not discard?) (not won?))
                (and discard? won?))
            (w/fail "Must play discard or go out but not both"))
   (w/modify #(dissoc % :played))
   next-turn <- (inc-pid turn)
   let [new-hand (-> hand
                     (update :down #(if down? down %))
                     (update :held #(->> %1
                                         (remove (partial contains? played-set))
                                         vec)))]
   (w/modify #(-> %1
                  (merge {:first-turn? false
                          :drawn? false
                          :discard (vec (concat (if discard? [discard] [])
                                                discard-pile))
                          :turn next-turn})
                  (assoc-in [:hands name] new-hand)))
   (w/whenm won?
            {:keys [dealer hands scores money]} <- w/get
            next-dealer <- (inc-pid dealer)
            let [updated-scores
                 (a/map-values (fn [curr name]
                                 (conj curr
                                       (reduce #(-> %2
                                                    c/from-int
                                                    c/to-points
                                                    (+ %1))
                                               0
                                               (get-in hands [name :held]))))
                               scores)
                 buy-in (if (= 0 hand-id) 25 10)
                 contrib
                 (a/map-values (fn [_ name]
                                 (+ buy-in
                                    (* 5
                                       (- 3
                                          (get-in hands [name :may-is])))))
                               money)
                 pot-size (reduce + 0 (vals contrib))
                 updated-money
                 (a/map-values (fn [curr name]
                                 (+ curr
                                    (if (= name turn-name) pot-size 0)
                                    (* -1
                                       (get contrib name))))
                               money)]
            (w/modify #(merge %1
                              {:money updated-money
                               :scores updated-scores
                               :dealer next-dealer}))
            (w/whenm (= hand-id 6)
                     {:keys [dealer hands scores money]} <- w/get
                     let [total-scores (a/map-values #(reduce + 0 %) scores)
                          [min-score] (->> total-scores
                                           (a/map-values vector)
                                           vals
                                           (apply min-key first))
                          tied (->> total-scores
                                    vals
                                    (filter #(= min-score %))
                                    count)
                          oweds
                          (a/map-values (fn [score]
                                          (* 5
                                             (js/Math.round (/ (- score
                                                                  min-score)
                                                               20))))
                                        total-scores)
                          total-owed (/ (reduce + 0 (vals oweds))
                                        tied)
                          final-money
                          (a/map-values (fn [curr name]
                                          (if (= min-score
                                                 (get total-scores name))
                                            (+ curr total-owed)
                                            (- curr (get oweds name))))
                                        money)]
                     (w/modify #(assoc %1 :money final-money)))))

  Deal
  (mdo
   {:keys [shuffle name]} <- w/ask
   {:keys [name-by-id
           hand
           dealer]} <- w/get
   turn <- (inc-pid dealer)
   let [dealer-name (get name-by-id dealer)]
   hand-over? <- get-hand-over?
   (w/whenm (not hand-over?)
            (w/fail "Current hand is not over"))
   (w/whenm (= 6 hand)
            (w/fail "Game is already over"))
   (w/whenm (not= name dealer-name)
            (w/fail "Not the dealer"))
   {:keys [discard deck hands]} <- distribute-cards
   (w/modify (a/curry merge
                      {:first-turn? true
                       :drawn? false
                       :hand (inc hand)
                       :turn turn
                       :discard-requests {}
                       :hands hands
                       :discard discard
                       :deck deck}))))
;; Responses
(deftagged GameState [state])
(deftagged Error [error])

(defprotomethod error? [this]
  Error true
  GameState false)

(defn filter-game-state [state name]
  (-> state
      (dissoc :deck)
      (assoc :deck-count (count (:deck state)))
      (update :hands
              (partial a/map-values
                       (fn [hand player]
                         (if (= player name)
                           hand
                           (-> hand
                               (dissoc :held)
                               (assoc :held-count (count (:held hand))))))))
      (assoc :name name)))
