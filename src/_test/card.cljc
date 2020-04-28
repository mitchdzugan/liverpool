(ns -test.card
  (:require #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [is deftest testing]])
            [allpa.core :as a]
            [card :as c]))

(deftest cards
  (testing "joker?"
    (is (= (c/joker? (c/->Suited :hearts 1))
           false))
    (is (= (c/joker? (c/->NoCard))
           false))
    (is (= (c/joker? (c/->Joker :red))
           true))
    )
  (testing "no-card?"
    (is (= (c/no-card? (c/->Suited :hearts 1))
           false))
    (is (= (c/no-card? (c/->NoCard))
           true))
    (is (= (c/no-card? (c/->Joker :red))
           false))
    )
  (testing "from-int"
    (is (= (c/from-int nil)
           (c/->NoCard)))
    (is (= (c/from-int 0)
           (c/->Suited :diamonds 0)))
    (is (= (c/from-int 13)
           (c/->Suited :hearts 0)))
    (is (= (c/from-int 26)
           (c/->Suited :spades 0)))
    (is (= (c/from-int 39)
           (c/->Suited :clubs 0)))
    (is (= (c/from-int 42)
           (c/->Suited :clubs 3)))
    (is (= (c/from-int 54)
           (c/->Suited :diamonds 0)))
    (is (= (c/from-int 52)
           (c/->Joker :red)))
    (is (= (c/from-int 53)
           (c/->Joker :black)))
    )
  )
