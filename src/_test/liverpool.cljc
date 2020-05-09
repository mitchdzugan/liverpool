(ns -test.liverpool
  (:require #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [is deftest testing]])
            [allpa.core :as a]
            [wayra.core :refer [exec]]
            [liverpool :as l]
            [lambdaisland.deep-diff2 :as ddiff]
            [clojure.data :as data]))

(def initial-state
  {:discard []
   :deck []
   :first-turn? false
   :config {:num-decks 2}
   :drawn? false
   :started? false
   :id-by-name {"A" 0
                "B" 0
                "C" 0}
   :name-by-id {}
   :money {}
   :scores {}
   :hand 0
   :dealer 0
   :turn 0
   :hands {}
   :discard-requests {}})

(def after-start
  {:first-turn? true
   :config {:num-decks 2}
   :drawn? false
   :started? true
   :id-by-name {"A" 2
                "B" 1
                "C" 0}
   :name-by-id {0 "C"
                1 "B"
                2 "A"}
   :money {"A" 0
           "B" 0
           "C" 0}
   :scores {"A" []
            "B" []
            "C" []}
   :hand 0
   :dealer 0
   :turn 1
   :hands {"C" {:down {}
                :may-is 3
                :held [37 34 31 28 25 22 19 16 13 10 7]}
           "B" {:down {}
                :may-is 3
                :held [39 36 33 30 27 24 21 18 15 12 9]}
           "A" {:down {}
                :may-is 3
                :held [38 35 32 29 26 23 20 17 14 11 8]}}
   :discard [6]
   :deck [5 4 3 2 1 0]
   :discard-requests {}})

(def after-start-filtered-for-b
  {:name "B"
   :first-turn? true
   :config {:num-decks 2}
   :drawn? false
   :started? true
   :id-by-name {"A" 2
                "B" 1
                "C" 0}
   :name-by-id {0 "C"
                1 "B"
                2 "A"}
   :money {"A" 0
           "B" 0
           "C" 0}
   :scores {"A" []
            "B" []
            "C" []}
   :hand 0
   :dealer 0
   :turn 1
   :hands {"C" {:down {}
                :may-is 3
                :held-count 11}
           "B" {:down {}
                :may-is 3
                :held [39 36 33 30 27 24 21 18 15 12 9]}
           "A" {:down {}
                :may-is 3
                :held-count 11}}
   :discard [6]
   :deck-count 6
   :discard-requests {}})

(def drawn (merge after-start {:drawn? true}))

(defn run-monad [m state overrides]
  (-> (exec {:reader (merge {:shuffle shuffle
                             :cards-per-deck 54}
                            overrides)
             :init-state state}
            m)))

(defn run-monad-result [m state overrides]
  (:result (run-monad m state overrides)))

(defn run-monad-state [m state overrides]
  (:state (run-monad m state overrides)))

(defn run-monad-error [m state overrides]
  (:error (run-monad m state overrides)))

(defn run-action-state [action state overrides]
  (run-monad-state (l/handle-game-action action) state overrides))

(defn run-action-error [action state overrides]
  (run-monad-error (l/handle-game-action action) state overrides))

(deftest get-hand-winner
  (testing "hand is over"
    (let [expected "A"
          actual (run-monad-result l/get-hand-winner
                                   (assoc-in after-start [:hands "A" :held] [])
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "hand is not over"
    (let [expected nil
          actual (run-monad-result l/get-hand-winner
                                   after-start
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected)))))


(deftest get-request-states
  (testing "all explicit pass"
    (let [expected
          [{:name "B" :request-state :pass}
           {:name "A" :request-state :pass}
           {:name "C" :request-state :pass}]
          actual (run-monad-result l/get-request-states
                                   (merge after-start
                                          {:discard-requests {"A" false
                                                              "B" false
                                                              "C" false}})
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "undeclared on first turn"
    (let [expected
          [{:name "B" :request-state :pass}
           {:name "A" :request-state :pass}
           {:name "C" :request-state :na}]
          actual (run-monad-result l/get-request-states
                                   (merge after-start
                                          {:discard-requests {"A" false
                                                              "B" false}})
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "undeclared discarder as pass on non-first turn"
    (let [expected
          [{:name "B" :request-state :pass}
           {:name "A" :request-state :pass}
           {:name "C" :request-state :pass}]
          actual (run-monad-result l/get-request-states
                                   (merge after-start
                                          {:first-turn? false
                                           :discard-requests {"A" false
                                                              "B" false}})
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "explicit discarder overrides pass on non-first turn"
    (let [expected
          [{:name "B" :request-state :pass}
           {:name "A" :request-state :pass}
           {:name "C" :request-state :request}]
          actual (run-monad-result l/get-request-states
                                   (merge after-start
                                          {:first-turn? false
                                           :discard-requests {"A" false
                                                              "B" false
                                                              "C" true}})
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "undeclared when down as pass"
    (let [expected
          [{:name "B" :request-state :pass}
           {:name "A" :request-state :pass}
           {:name "C" :request-state :pass}]
          actual (run-monad-result l/get-request-states
                                   (-> after-start
                                       (assoc :first-turn? false)
                                       (assoc :discard-requests {"B" false
                                                                 "C" false})
                                       (assoc-in [:hands "A" :down :run 0] [0 1 2 3]))
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "explicit when down overrides pass"
    (let [expected
          [{:name "B" :request-state :pass}
           {:name "A" :request-state :request}
           {:name "C" :request-state :pass}]
          actual (run-monad-result l/get-request-states
                                   (-> after-start
                                       (assoc :first-turn? false)
                                       (assoc :discard-requests {"A" true
                                                                 "B" false
                                                                 "C" false})
                                       (assoc-in [:hands "A" :down :run 0] [0 1 2 3]))
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "always pass when out of may is and not turn"
    (let [expected
          [{:name "B" :request-state :pass}
           {:name "A" :request-state :pass}
           {:name "C" :request-state :pass}]
          actual (run-monad-result l/get-request-states
                                   (-> after-start
                                       (assoc :first-turn? false)
                                       (assoc :discard-requests {"A" true
                                                                 "B" false
                                                                 "C" false})
                                       (assoc-in [:hands "A" :may-is] 0))
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "always allow request when turn"
    (let [expected
          [{:name "B" :request-state :request}
           {:name "A" :request-state :pass}
           {:name "C" :request-state :pass}]
          actual (run-monad-result l/get-request-states
                                   (-> after-start
                                       (assoc :first-turn? false)
                                       (assoc :discard-requests {"A" false
                                                                 "B" true
                                                                 "C" false})
                                       (assoc-in [:hands "B" :may-is] 0))
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected)))))

(deftest progress-when-possible-helper
  (testing "all pass w/ non-empty deck"
    (let [expected (-> after-start
                       (assoc :deck [4 3 2 1 0])
                       (assoc-in [:hands "B" :held]
                                 [39 36 33 30 27 24 21 18 15 12 9 5])
                       (assoc :drawn? true))
          m (l/progress-when-possible-helper [{:name "B" :request-state :pass}
                                              {:name "A" :request-state :pass}
                                              {:name "C" :request-state :pass}])
          actual (run-monad-state m after-start {:cards-per-deck 20
                                                 :shuffle reverse})]
      (is (= actual expected))))
  (testing "all pass w/ empty deck"
    (let [expected (-> after-start
                       (assoc :deck [2 1])
                       (assoc :discard [])
                       (assoc-in [:hands "B" :held]
                                 [39 36 33 30 27 24 21 18 15 12 9 0])
                       (assoc :drawn? true))


          m (l/progress-when-possible-helper [{:name "B" :request-state :pass}
                                              {:name "A" :request-state :pass}
                                              {:name "C" :request-state :pass}])
          init-state (merge after-start
                            {:deck [0]
                             :discard [1 2]})
          actual (run-monad-state m init-state {:cards-per-deck 20
                                                :shuffle reverse})]
      (is (= actual expected))))
  (testing "not all pass"
    (let [expected after-start
          m (l/progress-when-possible-helper [{:name "B" :request-state :pass}
                                              {:name "A" :request-state :na}
                                              {:name "C" :request-state :request}])
          actual (run-monad-state m after-start {:cards-per-deck 20
                                                 :shuffle reverse})]
      (is (= actual expected))))
  (testing "current turn requests discard"
    (let [expected (-> after-start
                       (assoc :discard [])
                       (assoc-in [:hands "B" :held]
                                 [39 36 33 30 27 24 21 18 15 12 9 6])
                       (assoc :drawn? true))
          m (l/progress-when-possible-helper [{:name "B" :request-state :request}
                                              {:name "A" :request-state :pass}
                                              {:name "C" :request-state :pass}])
          actual (run-monad-state m after-start {:cards-per-deck 20
                                                 :shuffle reverse})]
      (is (= actual expected))))
  (testing "granted may i"
    (let [expected (-> after-start
                       (assoc :deck [2 1 0])
                       (assoc :discard [151 152 153 154])
                       (assoc-in [:hands "A" :held]
                                 [38 35 32 29 26 23 20 17 14 11 8 6 5 4])
                       (assoc-in [:hands "A" :may-is] 2)
                       (assoc-in [:hands "B" :held]
                                 [39 36 33 30 27 24 21 18 15 12 9 3])
                       (assoc :drawn? true))
          init-state (merge after-start
                            {:discard [6 151 152 153 154]})
          m (l/progress-when-possible-helper [{:name "B" :request-state :pass}
                                              {:name "A" :request-state :request}
                                              {:name "C" :request-state :pass}])
          actual (run-monad-state m init-state {:cards-per-deck 20
                                                :shuffle reverse})]
      (is (= actual expected))))
  (testing "granted may i w/o enough cards in deck"
    (let [expected (-> after-start
                       (assoc :deck [2])
                       (assoc :discard [])
                       (assoc-in [:hands "A" :held]
                                 [38 35 32 29 26 23 20 17 14 11 8 1 0 4])
                       (assoc-in [:hands "A" :may-is] 2)
                       (assoc-in [:hands "B" :held]
                                 [39 36 33 30 27 24 21 18 15 12 9 3])
                       (assoc :drawn? true))
          m (l/progress-when-possible-helper [{:name "B" :request-state :pass}
                                              {:name "A" :request-state :request}
                                              {:name "C" :request-state :pass}])
          init-state (merge after-start
                            {:deck [0]
                             :discard [1 2 3 4]})
          actual (run-monad-state m init-state {:cards-per-deck 20
                                                 :shuffle reverse})]
      (is (= actual expected))))
  (testing "granted may i w/ 3 cards in deck"
    (let [expected (-> after-start
                       (assoc :deck [5 4])
                       (assoc :discard [])
                       (assoc-in [:hands "A" :held]
                                 [38 35 32 29 26 23 20 17 14 11 8 3 0 1])
                       (assoc-in [:hands "A" :may-is] 2)
                       (assoc-in [:hands "B" :held]
                                 [39 36 33 30 27 24 21 18 15 12 9 2])
                       (assoc :drawn? true))
          m (l/progress-when-possible-helper [{:name "B" :request-state :pass}
                                              {:name "A" :request-state :request}
                                              {:name "C" :request-state :pass}])
          init-state (merge after-start
                            {:deck [0 1 2]
                             :discard [3 4 5]})
          actual (run-monad-state m init-state {:cards-per-deck 20
                                                 :shuffle reverse})]
      (is (= actual expected))))
  (testing "granted may i w/ 4 cards in deck"
    (let [expected (-> after-start
                       (assoc :deck [3])
                       (assoc :discard [5 6])
                       (assoc-in [:hands "A" :held]
                                 [38 35 32 29 26 23 20 17 14 11 8 4 0 1])
                       (assoc-in [:hands "A" :may-is] 2)
                       (assoc-in [:hands "B" :held]
                                 [39 36 33 30 27 24 21 18 15 12 9 2])
                       (assoc :drawn? true))
          m (l/progress-when-possible-helper [{:name "B" :request-state :pass}
                                              {:name "A" :request-state :request}
                                              {:name "C" :request-state :pass}])
          init-state (merge after-start
                            {:deck [0 1 2 3]
                             :discard [4 5 6]})
          actual (run-monad-state m init-state {:cards-per-deck 20
                                                 :shuffle reverse})]
      (is (= actual expected)))))

(deftest start-game
  (testing "start game"
    (let [expected after-start
          actual (run-action-state (l/->StartGame 0)
                                   initial-state
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "start started game"
    (let [expected "game already started"
          actual (run-action-error (l/->StartGame 0)
                                   after-start
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected)))))

(deftest pass-discard
  (testing "pass after draw"
    (let [expected "Player has already drawn"
          actual (run-action-error (l/->PassDiscard 0)
                                   drawn
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "pass"
    (let [expected (merge after-start
                          {:discard-requests {"A" false}})
          actual (run-action-state (l/->PassDiscard 0)
                                   after-start
                                   {:name "A"
                                    :cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected)))))

(deftest request-discard
  (testing "request after draw"
    (let [expected "Player has already drawn"
          actual (run-action-error (l/->RequestDiscard 0)
                                   drawn
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "request"
    (let [expected (merge after-start
                          {:discard-requests {"A" true}})
          actual (run-action-state (l/->RequestDiscard 0)
                                   after-start
                                   {:name "A"
                                    :cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected)))))

(deftest cancel
  (testing "cancel after draw"
    (let [expected "Player has already drawn"
          actual (run-action-error (l/->Cancel 0)
                                   drawn
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "cancel"
    (let [expected after-start
          actual (run-action-state (l/->Cancel 0)
                                   (merge after-start
                                          {:discard-requests {"A" true}})
                                   {:name "A"
                                    :cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected)))))

(deftest validate-play
  (testing "throws on non-cards"
    (let [expected "Play contained non-card"
          actual-1 (run-monad-error (l/validate-play :run [0 -1 1])
                                    after-start
                                    {:cards-per-deck 20})
          actual-2 (run-monad-error (l/validate-play :run [0 "1" 1])
                                    after-start
                                    {:cards-per-deck 20})]
      (is (= actual-1 expected))
      (is (= actual-2 expected))))
  (testing "throws on card out of range"
    (let [expected "Play contained card outside of decks"
          actual (run-monad-error (l/validate-play :run [0 40 1])
                                  after-start
                                  {:cards-per-deck 20})]
      (is (= actual expected))))
  (testing "throws on card out of range"
    (let [expected "Play contained duplicate cards"
          actual (run-monad-error (l/validate-play :run [0 0 1])
                                  after-start
                                  {:cards-per-deck 20})]
      (is (= actual expected))))
  (testing "throws on all jokers"
    (let [expected "Play cannot contain all jokers"
          actual (run-monad-error (l/validate-play :set [52 53 107])
                                  (merge after-start
                                         {:hands {"A" {:held [52 53 107]}}})
                                  {:name "A"})]
      (is (= actual expected))))
  (testing "throws on invalid play type"
    (let [expected "Invalid play type"
          actual (run-monad-error (l/validate-play :sss [50 53 107])
                                  (merge after-start
                                         {:hands {"A" {:held [50 53 107]}}})
                                  {:name "A"})]
      (is (= actual expected))))
  (testing "throws on set with multiple ranks"
    (let [expected "Set play contained multiple ranks"
          actual (run-monad-error (l/validate-play :set [0 13 25])
                                  (merge after-start
                                         {:hands {"A" {:held [0 13 25]}}})
                                  {:name "A"})]
      (is (= actual expected))))
  (testing "throws on set with not enough cards"
    (let [expected "Set play did not contain at least 3 cards"
          actual (run-monad-error (l/validate-play :set [0 13])
                                  (merge after-start
                                         {:hands {"A" {:held [0 13 25]}}})
                                  {:name "A"})]
      (is (= actual expected))))
  (testing "passes on valid set play"
    (let [expected nil
          actual (run-monad-error (l/validate-play :set [0 13 26])
                                  (merge after-start
                                         {:hands {"A" {:held [0 13 26]}}})
                                  {:name "A"})]
      (is (= actual expected))))
  (testing "passes on valid set play with jokers"
    (let [expected nil
          actual (run-monad-error (l/validate-play :set [0 13 53])
                                  (merge after-start
                                         {:hands {"A" {:held [0 13 53]}}})
                                  {:name "A"})]
      (is (= actual expected))))
  (testing "throws on run with multiple suits"
    (let [expected "Run play contained multiple suits"
          actual (run-monad-error (l/validate-play :run [0 1 2 13])
                                  (merge after-start
                                         {:hands {"A" {:held [0 1 2 13]}}})
                                  {:name "A"})]
      (is (= actual expected))))
  (testing "throws on run with not increasing order"
    (let [expected "Run play did not contain ordered ranks"
          actual (run-monad-error (l/validate-play :run [0 1 2 4])
                                  (merge after-start
                                         {:hands {"A" {:held [0 1 2 4]}}})
                                  {:name "A"})]
      (is (= actual expected))))
  (testing "throws on run with not enough cards"
    (let [expected "Run play did not contain at least 4 cards"
          actual (run-monad-error (l/validate-play :run [0 1 2])
                                  (merge after-start
                                         {:hands {"A" {:held [0 1 2 4]}}})
                                  {:name "A"})]
      (is (= actual expected))))
  (testing "throws on run with ace as high and low"
    (let [expected "Run play did not contain ordered ranks"
          actual (run-monad-error (l/validate-play :run [11 12 0 1])
                                  (merge after-start
                                         {:hands {"A" {:held [11 12 0 1]}}})
                                  {:name "A"})]
      (is (= actual expected))))
  (testing "throws on run with ace as high and low and joker"
    (let [expected "Run play did not contain ordered ranks"
          actual (run-monad-error (l/validate-play :run [11 12 0 53])
                                  (merge after-start
                                         {:hands {"A" {:held [11 12 0 1 53]}}})
                                  {:name "A"})]
      (is (= actual expected))))
  (testing "passes on valid run play low ace"
    (let [expected nil
          actual (run-monad-error (l/validate-play :run [0 1 2 3])
                                  (merge after-start
                                         {:hands {"A" {:held [0 1 2 3]}}})
                                  {:name "A"})]
      (is (= actual expected))))
  (testing "passes on valid run play with high ace"
    (let [expected nil
          actual (run-monad-error (l/validate-play :run [10 11 12 0])
                                  (merge after-start
                                         {:hands {"A" {:held [10 11 12 0]}}})
                                  {:name "A"})]
      (is (= actual expected))))
  (testing "passes on valid run play reversed"
    (let [expected nil
          actual (run-monad-error (l/validate-play :run [5 4 3 2])
                                  (merge after-start
                                         {:hands {"A" {:held [5 4 3 2]}}})
                                  {:name "A"})]
      (is (= actual expected))))
  (testing "passes on valid run play with jokers"
    (let [expected nil
          actual (run-monad-error (l/validate-play :run [2 3 53 5])
                                  (merge after-start
                                         {:hands {"A" {:held [53 5 4 3 2]}}})
                                  {:name "A"})]
      (is (= actual expected))))
  (testing "passes on valid run play with jokers at start"
    (let [expected nil
          actual (run-monad-error (l/validate-play :run [53 2 3 4])
                                  (merge after-start
                                         {:hands {"A" {:held [53 5 4 3 2]}}})
                                  {:name "A"})]
      (is (= actual expected)))))

(def after-draw
  (merge after-start
         {:drawn? true}))

(deftest play
  (testing "Throws when play before draw"
    (let [expected "Cannot play until done drawing"
          actual (run-action-error (l/->Play 0 {})
                                   after-start
                                   {:name "B"
                                    :cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "Throws when play on wrong turn"
    (let [expected "Only player whose turn it is can play"
          actual (run-action-error (l/->Play 0 {})
                                   after-draw
                                   {:name "A"
                                    :cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "Throws when down play and already down"
    (let [expected "Already down"
          actual (run-action-error (l/->Play 0 {:down {:run [[8 9 10 11]]}})
                                   (assoc-in after-draw
                                             [:hands "B" :down]
                                             {:run [[0 1 2 3]
                                                    [4 5 6 7]]})
                                   {:name "B"
                                    :cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "Throws when table play before down"
    (let [expected "Cannot play on others until down"
          actual (run-action-error (l/->Play 0 {:table {"A" {:run {0 [[8 9 10 11]
                                                                      []]}}}})
                                   after-draw
                                   {:name "B"
                                    :cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "Throws when table play on invalid target"
    (let [expected "Playing on invalid target"
          actual (run-action-error (l/->Play 0 {:down {:set [[0 13 26]
                                                             [1 14 27]]}
                                                :table {"A" {:run {0 [[8 9 10 11]
                                                                      []]}}}})
                                   (assoc-in after-draw
                                             [:hands "B" :held]
                                             [0 1 13 14 26 27 8 9 10 11 12 13])
                                   {:name "B"
                                    :cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "Throws on invalid down play"
    (let [expected "Set play contained multiple ranks"
          actual (run-action-error (l/->Play 0 {:down {:set [[2 13 26]
                                                             [1 14 27]]}})
                                   (assoc-in after-draw
                                             [:hands "B" :held]
                                             [2 1 13 14 26 27 8 9 10 11 12 13])
                                   {:name "B"
                                    :cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "Throws on table play with post cards causing invalid play"
    (let [expected "Run play did not contain ordered ranks"
          actual (run-action-error (l/->Play 0 {:down {:set [[0 13 26]
                                                             [1 14 27]]}
                                                :table {"A" {:run {0 [[]
                                                                      [7 8]]}}}})
                                   (-> after-draw
                                       (assoc-in [:hands "B" :held]
                                                 [0 1 7 8 13 14 26 27 8 9 10 11 12 13])
                                       (assoc-in [:hands "A" :down]
                                                 {:run [[2 3 4 5]]}))
                                   {:name "B"
                                    :cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "Throws on down with not enough sets"
    (let [expected "Not enough sets played"
          actual (run-action-error (l/->Play 0 {:down {:set [[0 13 26]]}})
                                   (-> after-draw
                                       (assoc-in [:hands "B" :held]
                                                 [0 1 7 8 13 14 26 27 8 9 10 11 12 13])
                                       (assoc-in [:hands "A" :down]
                                                 {:run [[2 3 4 5]]}))
                                   {:name "B"
                                    :cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))

  (testing "Throws on down with not enough sets"
    (let [expected "Not enough runs played"
          actual (run-action-error (l/->Play 0 {:down {:set [[0 13 26]]}})
                                   (-> after-draw
                                       (assoc :hand 1)
                                       (assoc-in [:hands "B" :held]
                                                 [0 1 7 8 13 14 26 27 8 9 10 11 12 13])
                                       (assoc-in [:hands "A" :down]
                                                 {:run [[2 3 4 5]]}))
                                   {:name "B"
                                    :cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "Throws on down with card not in hand"
    (let [expected "Play contained card not in hand"
          actual (run-action-error (l/->Play 0 {:down {:set [[0 13 26]
                                                             [1 14 27]]}})
                                   (-> after-draw
                                       (assoc-in [:hands "B" :held]
                                                 [0 7 8 13 14 26 27 8 9 10 11 12 13])
                                       (assoc-in [:hands "A" :down]
                                                 {:run [[2 3 4 5]]}))
                                   {:name "B"
                                    :cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "Throws on down with duplicate card used in different plays"
    (let [expected "Used duplicate cards in play"
          actual (run-action-error (l/->Play 0 {:down {:set [[0 13 26]
                                                             [0 13 26]]}})
                                   (-> after-draw
                                       (assoc-in [:hands "B" :held]
                                                 [0 7 8 13 14 26 27 8 9 10 11 12 13])
                                       (assoc-in [:hands "A" :down]
                                                 {:run [[2 3 4 5]]}))
                                   {:name "B"
                                    :cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "no discard"
    (let [expected "Must play discard or go out but not both"
          actual (run-action-error (l/->Play 0 {:down {:set [[0 13 26]
                                                             [1 14 27]]}})
                                   (-> after-draw
                                       (assoc-in [:hands "B" :held]
                                                 [0 1 7 8 13 14 26 27 8 9 10 11 12 13])
                                       (assoc-in [:hands "A" :down]
                                                 {:run [[2 3 4 5]]}))
                                   {:name "B"
                                    :cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "win via discard"
    (let [expected "Must play discard or go out but not both"
          actual (run-action-error (l/->Play 0 {:discard 9
                                                :down {:set [[0 13 26]
                                                             [1 14 27]]}})
                                   (-> after-draw
                                       (assoc-in [:hands "B" :held]
                                                 [0 1 13 14 26 27 9])
                                       (assoc-in [:hands "A" :down]
                                                 {:run [[2 3 4 5]]}))
                                   {:name "B"
                                    :cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "succesful play"
    (let [init-state (-> after-draw
                         (assoc-in [:hands "B" :held]
                                   [0 1 13 14 26 27 41 53 9 10])
                         (assoc-in [:hands "A" :down]
                                   {:set [[2 15 28]
                                          [3 16 29]]}))
          expected (-> init-state
                       (assoc :drawn? false)
                       (assoc :first-turn? false)
                       (assoc :turn 2)
                       (assoc :discard [9 6])
                       (assoc-in [:hands "B"]
                                 {:may-is 3
                                  :down {:set [[0 13 26]
                                               [1 14 27]]}
                                  :held [10]})
                       (assoc-in [:hands "A" :down :set 0]
                                 [53 2 15 28 41]))
          actual (run-action-state (l/->Play 0 {:discard 9
                                                :down {:set [[0 13 26]
                                                             [1 14 27]]}
                                                :table {"A" {:set {0 [[53]
                                                                      [41]]}}}})
                                   init-state
                                   {:name "B"
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "succesful play with no down"
    (let [init-state (-> after-draw
                         (assoc-in [:hands "B"]
                                   {:may-is 3
                                    :held [41 53 9 10]
                                    :down {:set [[0 13 26]
                                                 [1 14 27]]}})
                         (assoc-in [:hands "A" :down]
                                   {:set [[2 15 28]
                                          [3 16 29]]}))
          expected (-> init-state
                       (assoc :drawn? false)
                       (assoc :first-turn? false)
                       (assoc :turn 2)
                       (assoc :discard [9 6])
                       (assoc-in [:hands "B"]
                                 {:may-is 3
                                  :down {:set [[0 13 26]
                                               [1 14 27]]}
                                  :held [10]})
                       (assoc-in [:hands "A" :down :set 0]
                                 [53 2 15 28 41]))
          actual (run-action-state (l/->Play 0 {:discard 9
                                                :table {"A" {:set {0 [[53]
                                                                      [41]]}}}})
                                   init-state
                                   {:name "B"
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "round win"
    (let [init-state (-> after-draw
                         (assoc-in [:scores "A"] [10])
                         (assoc-in [:money "A"] -10)
                         (assoc-in [:money "B"] 10)
                         (assoc-in [:hands "B"]
                                   {:may-is 1
                                    :held [41 53]
                                    :down {:set [[0 13 26]
                                                 [1 14 27]]}})
                         (assoc-in [:hands "A" :may-is] 1)
                         (assoc-in [:hands "A" :down]
                                   {:set [[2 15 28]
                                          [3 16 29]]}))
          expected (-> init-state
                       (assoc :drawn? false)
                       (assoc :first-turn? false)
                       (assoc :turn 2)
                       (assoc :dealer 1)
                       (assoc-in [:hands "B"]
                                 {:may-is 1
                                  :down {:set [[0 13 26]
                                               [1 14 27]]}
                                  :held []})
                       (assoc-in [:hands "A" :down :set 0]
                                 [53 2 15 28 41])
                       (assoc :scores {"A" [10 100]
                                       "B" [0]
                                       "C" [100]})
                       (assoc :money {"A" -45
                                      "B" 70
                                      "C" -25}))
          actual (run-action-state (l/->Play 0 {:table {"A" {:set {0 [[53]
                                                                      [41]]}}}})
                                   init-state
                                   {:name "B"
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "round win"
    (let [init-state (-> after-draw
                         (assoc :hand 2)
                         (assoc-in [:scores "A"] [10])
                         (assoc-in [:money "A"] -10)
                         (assoc-in [:money "B"] 10)
                         (assoc-in [:hands "B"]
                                   {:may-is 1
                                    :held [41 53]
                                    :down {:set [[0 13 26]
                                                 [1 14 27]]}})
                         (assoc-in [:hands "A" :may-is] 1)
                         (assoc-in [:hands "A" :down]
                                   {:set [[2 15 28]
                                          [3 16 29]]}))
          expected (-> init-state
                       (assoc :drawn? false)
                       (assoc :first-turn? false)
                       (assoc :turn 2)
                       (assoc :dealer 1)
                       (assoc-in [:hands "B"]
                                 {:may-is 1
                                  :down {:set [[0 13 26]
                                               [1 14 27]]}
                                  :held []})
                       (assoc-in [:hands "A" :down :set 0]
                                 [53 2 15 28 41])
                       (assoc :scores {"A" [10 100]
                                       "B" [0]
                                       "C" [100]})
                       (assoc :money {"A" -30
                                      "B" 40
                                      "C" -10}))
          actual (run-action-state (l/->Play 0 {:table {"A" {:set {0 [[53]
                                                                      [41]]}}}})
                                   init-state
                                   {:name "B"
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "game over"
    (let [init-state (-> after-draw
                         (assoc :hand 6)
                         (assoc-in [:scores "A"] [10])
                         (assoc-in [:scores "B"] [20])
                         (assoc-in [:money "A"] -10)
                         (assoc-in [:money "B"] 10)
                         (assoc-in [:hands "B"]
                                   {:may-is 1
                                    :held [41 53]
                                    :down {:set [[0 13 26]
                                                 [1 14 27]]}})
                         (assoc-in [:hands "A" :may-is] 1)
                         (assoc-in [:hands "A" :down]
                                   {:set [[2 15 28]
                                          [3 16 29]]}))
          expected (-> init-state
                       (assoc :drawn? false)
                       (assoc :first-turn? false)
                       (assoc :turn 2)
                       (assoc :dealer 1)
                       (assoc-in [:hands "B"]
                                 {:may-is 1
                                  :down {:set [[0 13 26]
                                               [1 14 27]]}
                                  :held []})
                       (assoc-in [:hands "A" :down :set 0]
                                 [53 2 15 28 41])
                       (assoc :scores {"A" [10 100]
                                       "B" [20 0]
                                       "C" [100]})
                       (assoc :money {"A" -55
                                      "B" 85
                                      "C" -30}))
          actual (run-action-state (l/->Play 0 {:table {"A" {:set {0 [[53]
                                                                      [41]]}}}})
                                   init-state
                                   {:name "B"
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "game over with tie"
    (let [init-state (-> after-draw
                         (assoc :hand 6)
                         (assoc-in [:scores "A"] [10])
                         (assoc-in [:scores "B"] [100])
                         (assoc-in [:money "A"] -10)
                         (assoc-in [:money "B"] 10)
                         (assoc-in [:hands "B"]
                                   {:may-is 1
                                    :held [41 53]
                                    :down {:set [[0 13 26]
                                                 [1 14 27]]}})
                         (assoc-in [:hands "A" :may-is] 1)
                         (assoc-in [:hands "A" :down]
                                   {:set [[2 15 28]
                                          [3 16 29]]}))
          expected (-> init-state
                       (assoc :drawn? false)
                       (assoc :first-turn? false)
                       (assoc :turn 2)
                       (assoc :dealer 1)
                       (assoc-in [:hands "B"]
                                 {:may-is 1
                                  :down {:set [[0 13 26]
                                               [1 14 27]]}
                                  :held []})
                       (assoc-in [:hands "A" :down :set 0]
                                 [53 2 15 28 41])
                       (assoc :scores {"A" [10 100]
                                       "B" [100 0]
                                       "C" [100]})
                       (assoc :money {"A" -35
                                      "B" 42.5
                                      "C" -7.5}))
          actual (run-action-state (l/->Play 0 {:table {"A" {:set {0 [[53]
                                                                      [41]]}}}})
                                   init-state
                                   {:name "B"
                                    :shuffle reverse})]
      (is (= actual expected)))))

(deftest deal
  (testing "deal before end of hand"
    (let [expected "Current hand is not over"
          actual (run-action-error (l/->Deal 0)
                                   after-start
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "deal after game overe"
    (let [expected "Game is already over"
          actual (run-action-error (l/->Deal 0)
                                   (-> after-start
                                       (assoc-in [:hands "A" :held] [])
                                       (assoc :hand 6))
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "deal after game over"
    (let [expected "Not the dealer"
          actual (run-action-error (l/->Deal 0)
                                   (-> after-start
                                       (assoc-in [:hands "A" :held] []))
                                   {:name "B"
                                    :cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "deal"
    (let [expected (assoc after-start :hand 1)
          actual (run-action-state (l/->Deal 0)
                                   (-> after-start
                                       (assoc :deck [1 23 37])
                                       (assoc :discard [39 38 33])
                                       (assoc-in [:hands "A" :held] []))
                                   {:name "C"
                                    :cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected)))))

(deftest join-room
  (testing "throws on join in progress game"
    (let [expected "Game already in progress"
          actual (run-action-error (l/->JoinRoom 0 "D")
                                   after-start
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "join unstarted game"
    (let [expected (-> initial-state
                       (assoc-in [:id-by-name "D"] 0))
          actual (run-action-state (l/->JoinRoom 0 "D")
                                   initial-state
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected)))))

(deftest configure-room
  (testing "throws on configure in progress game"
    (let [expected "Game already in progress"
          actual (run-action-error (l/->ConfigureRoom 0 {:num-decks 3})
                                   after-start
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected))))
  (testing "configure unstarted game"
    (let [expected (-> initial-state
                       (assoc :config {:num-decks 3}))
          actual (run-action-state (l/->ConfigureRoom 0 {:num-decks 3})
                                   initial-state
                                   {:cards-per-deck 20
                                    :shuffle reverse})]
      (is (= actual expected)))))

(deftest filter-game-state
  (testing "filters our secret information"
    (let [expected after-start-filtered-for-b
          actual (l/filter-game-state after-start "B")]
      (is (= actual expected)))))
