(ns ui.entry
  (:require [mayu.macros :refer [defui ui]]
            [mayu.frp.event :as e]
            [mayu.frp.signal :as s]
            [mayu.dom :as dom]
            [router :as r]))

(defn browser-fn [a]
  (* 6 a))

(defui page []
  {:keys [path-params]} <- (dom/envs ::r/route)
  let [{:keys [id]} path-params]
  <[h2 (str "Page " id)]
  <[a {:href (r/url ::r/home)} "Home"])

(defui btn-skip-every-other-click [text]
  btn <- (dom/create-element "button" text)
  [(->> (dom/on-click btn)
        (e/reduce (fn [{:keys [skip?]} val]
                    {:skip? (not skip?)
                     :val val})
                  {:skip? false})
        (e/filter #(= false (:skip? %1)))
        (e/map :val)
        (e/map #(-> 1))
        (e/reduce + 0)
        )])

(defui home []
  <[dom/collect-and-reduce ::counter + 0 $=

    <[button "subtract 1 to score"] btn2 >
    (dom/emit ::counter (->> (dom/on-click btn2)
                             (e/map #(-> -1))))
    <[button "add 2 to score"] btn3 >
    (dom/emit ::counter (->> (dom/on-click btn3)
                             (e/map #(-> 2))))
    s-counter <- (dom/envs ::counter)
    <[dom/bind s-counter
      (fn [counter]
        (dom/create-element "p" (str "Current count: " counter)))]]


  <[button "Add 1 to score"] btn1 >
  (dom/emit ::counter (->> (dom/on-click btn1)
                           (e/map #(-> 1))))

  <[div $=
    <[h1 "Home"]
    <[ul $=
      <[li $=
        <[a {:href (r/url ::r/page {:id 1})} "Page 1"]]
      <[li $=
        <[a {:href (r/url ::r/page {:id 2})} "Page 2"]]
      <[li $=
        <[a {:href (r/url ::r/page {:id 3})} "Page 3"]]]])

(defui root []
  s-route <- (dom/envs ::r/s-route)
  <[dom/bind s-route $[route]=
    <[dom/assoc-env ::r/route route $=
      let [name (get-in route [:data :name])]
      <[case name
        <[::r/home <[home]]
        <[::r/page <[page]]]]])
