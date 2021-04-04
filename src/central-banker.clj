(ns central-banker.main)

(set! *warn-on-reflection* true) ;; avoid reflexion so you can use graalvm

;; for testing
(def start-econ
  {
   :r 5,
   :pi 2,
   :u 5
   :y 100, :ye 100
   :quarter 0
   :last-event "Business as usual"
   })

;; the model should be a function that does the normal actions each quarter
;;

(defn- event-effect [& rest]
    (fn [econ]
      (apply update econ rest)))

(def events
  [{
    :description "Business as usual"
    :function identity
    :probability 0.90 ;;used to be 0.83
    },
   {
    :description "Oil crisis"
    :function (event-effect :pi + 10)
    :probability 0.01
    },
   {
    :description "Stock market crash"
    :function (comp (event-effect :pi - 4)
                    (event-effect :y - 10))
    :probability 0.05
    },
   {
    :description "Real-estate boom"
    :function (comp (event-effect :pi + 3)
                    (event-effect :y + 5))
    :probability 0.01
    },
   {
    :description "Productivity boom"
    :function (comp (event-effect :pi - 5)
                    (event-effect :ye + 10))
    :probability 0.01
    },
   {
    :description "Natural disaster"
    :function (comp (event-effect :ye - 5)
                    (event-effect :pi + 9)
                    (event-effect :u - 9))
    :probability 0.01
    },
   {
    :description "Income Tax Cut"
    :function (comp (event-effect :y + 4)
                    (event-effect :ye + 2))
    :probability 0.01
    }
   ])

;; Rich Hickey made this function, so I'm including his notice
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ant sim ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Copyright (c) Rich Hickey. All rights reserved.
;;
;;   The use and distribution terms for this software are covered by the
;;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;;   which can be found in the file CPL.TXT at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;
;;   You must not remove this notice, or any other, from this software.
(defn- wrand 
  "given a vector of slice sizes, returns the index of a slice given a
  random spin of a roulette wheel with compartments proportional to
  slices."
  [slices]
  (let [total (reduce + slices)
        r (rand total)]
    (loop [i 0 sum 0]
      (if (< r (+ (slices i) sum))
        i
        (recur (inc i) (+ (slices i) sum))))))

;; recur arg for primitive local: sum is not matching primitive, had: Object, needed: long
;; Auto-boxing loop arg: sum

(defn- rand-event
  "Returns a random event weighted by :probability"
  [events]
  (->> (map :probability events)
       vec
       wrand
       events))

(comment
  "Test the randomness of events"
  (do
    (use 'clojure.pprint)
    (print-table (take 16 (repeatedly (fn [] (rand-event events))))))
  ,)

;; update with weighted probability
(defn event-update
  "Weighted random event function applied to the economy, unpure print for debugging"
  [econ]
  (let [r (rand-event events)]
    ((comp
      (:function r)
      (event-effect :last-event
                    (fn [_] (:description r))))
     econ)))

(defn set-policy []
  (Integer/parseInt (read-line)))

(defn model [econ r]
  (letfn [(add-abs [o & rest]
            (let [res (apply + o rest)]
              (if (neg? res) 0 res)))
          (is [r]
            "returns the y₁; variant of the investment-saving curve from IS-LM"
            (add-abs - 100 (* 0.25 r)))
          (pc [pi r]
            "returns the π₁, Phillips Curve variant"
            (+ pi 5 (* 4 (- (is r) ye))))
          (jobsearch [u pi]
            (add-abs u (* -0.5 pi)))]
    (let [{:keys [ye y pi]} econ]
      (-> econ
          (assoc :y (is r))
          (update :pi pc r) ;; uses ye
          (update :u jobsearch pi)
          ;; add something for unemployment
          ,))))

(defn pass-quarter
  "update the economy map with the events and policy"
  [econ r]
  (-> econ
      event-update
      (model r)
      (assoc :r r)
      (update :quarter inc)))


(defn game-report! [history]
  (do
    (println "Here is your game report:")
    (clojure.pprint/print-table [:quarter :i :p :u :last-event] history)))

;; a variation of the Misery index by Arthur Okun
;; is used to calculate if you win the game

(defn game-over [history]
  "See if the game is won or lost"
  (let [p (map :pi history)
        u (map :u history)
        avg (fn [coll] (/ (apply + 0.0 coll)
                          (count coll)))
        misery (+ (avg p)
                  (avg u))]
    (game-report! history)
    (println "Misery Index: " misery)
    (if  (<= misery 10.0)
      "You won the game!"
      "Game over, you lost!")))


(comment
  ;; test a passing quarter
  (def example-econ (atom start-econ))
  (swap! example-econ pass-quarter 0)
  (reset! example-econ start-econ)
  ;; show 16 quarters saved in a var called hist in a table
  (do
    (reset! example-econ start-econ)
    (def hist
      (vec (take 16 (repeatedly #(swap! example-econ pass-quarter 5)))))
    (clojure.pprint/print-table hist))
  ,)


;; TODO:  use destructuring in let
;; TODO: Separate presentation function
(defn game
  "The game function waits for a policy (interest rate - i)
  to be applied in response to the state of the economy"
  [length]
  (loop [econ start-econ, history []]
    (let [current-quarter (:quarter econ)]
      (println
       "r:" (:r econ)
       "Current quarter: " current-quarter
       (:last-event econ) "\n"
       " Inflation: " (:pi econ)
       " Unemployment: " (:u econ)
       " enter your target r:")
      (if (< current-quarter length)
        (recur (pass-quarter econ (set-policy)) , (conj history econ))
        (game-over history)))))
