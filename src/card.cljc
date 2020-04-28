(ns card
  (:require [allpa.core
             :refer [deftagged defprotomethod]]))

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
