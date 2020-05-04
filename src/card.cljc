(ns card
  (:require [allpa.core
             :refer [deftagged defprotomethod]])
  )

(deftagged Suited [suit n])
(deftagged Joker [color])
(deftagged NoCard [])

(defprotomethod joker? [_]
  [Suited NoCard]
  false
  Joker
  true)

(defprotomethod no-card? [_]
  [Suited Joker]
  false
  NoCard
  true)

(defprotomethod to-src [{:keys [color suit n]}]
  Suited
  (case suit
    :diamonds (str "/cards/" n "." "0.png")
    :hearts (str "/cards/" n "." "1.png")
    :spades (str "/cards/" n "." "2.png")
    :clubs (str "/cards/" n "." "3.png"))

  NoCard
  "/cards/15.3.png"

  Joker
  (case color
    :red "/cards/15.1.png"
    :black "/cards/15.2.png"))

(defprotomethod to-points [{:keys [n]}]
  NoCard 0
  Joker 50
  Suited
  (cond
    (= 0 n) 20
    (< n 7) 5
    :else 10))

(defn from-int [n]
  (if (not= n (int n))
    (->NoCard)
    (let [mod54 (mod n 54)]
      (case mod54
        52 (->Joker :red)
        53 (->Joker :black)
        (->Suited (case (quot mod54 13)
                    0 :diamonds
                    1 :hearts
                    2 :spades
                    3 :clubs)
                  (mod mod54 13))))))
