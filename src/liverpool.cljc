(ns liverpool
  (:require [allpa.core
             :refer [deftagged defprotomethod]]))

;; Requests
(deftagged CreateRoom [name])
(deftagged JoinRoom [room-id name])
(deftagged ConfigureRoom [room-id options])
(deftagged StartGame [room-id])
(deftagged Pass [room-id])
(deftagged MayI [room-id])
(deftagged UnMayI [room-id])
(deftagged TakeDiscard [room-id])
(deftagged Unintend [room-id])
(deftagged DrawDeck [room-id])
(deftagged Play [room-id plays])
(deftagged Deal [room-id])
(deftagged Ping [room-id])

;; Responses
(deftagged GameState [room-id state])
(deftagged Error [error])
