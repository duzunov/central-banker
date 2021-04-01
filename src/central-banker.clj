(ns central-banker.main)

(set! *warn-on-reflection* true) ;; avoid reflexion so you can use graalvm

;; for testing
(def start-econ
  {
   :i 5, :i* 0
   :p 5, :p* 0
   :u 5, :u* 0
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
    :probability 0.83
    },
   {
    :description "Oil crisis"
    :function (event-effect :p + 10)
    :probability 0.01
    },
   {
    :description "Stock market crash"
    :function (comp (event-effect :p - 4)
                    (event-effect :u + 10))
    :probability 0.05
    },
   {
    :description "Real-estate boom"
    :function (comp (event-effect :p + 3)
                    (event-effect :u - 1))
    :probability 0.05
    },
   {
    :description "Productivity boom"
    :function (comp (event-effect :p - 5)
                    (event-effect :u - 5))
    :probability 0.04
    },
   {
    :description "Natural disaster"
    :function (comp (event-effect :p + 9)
                    (event-effect :u - 9))
    :probability 0.01
    },
   {
    :description "Income Tax Cut"
    :function (event-effect :p + 4)
    :probability 0.01
    }
   ])

;; Rich Hickey made this function
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
  (use 'clojure.pprint)
  (print-table (take 10 (repeatedly (fn [] (rand-event events)))))
  ,)

;; update with weighted probability
(defn event-update
  "Weighted random event function applied to the economy, unpure print for debugging"
  [econ]
  (let [r (rand-event events)]
    (println (:description r)) ;; Should I use tap> instead of this?
    ((:function r) econ)))

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

(comment
  (set-policy)
  )

;; Note that this model doesn't allow for deflation which is pretty rare anyway
(defn model [econ i]
  (letfn [(add-abs [o & rest]
            (let [res (apply + o rest)]
              (if (neg? res) 0 res)))]
    (-> econ
        (update :p add-abs (:p* econ))
        (update :u add-abs (:u* econ))
        (update :p* - i)
        (update :u* + i))))

(defn pass-quarter
  "update the economy map with the events (DOTO functions) and policy"
  [econ i]
  (-> econ
      event-update
      (model i)
      (update :quarter inc)
      ))

;; TODO add unemployment
;; scoring for example
;; change this to accept a sequence of econ maps
(defn game-over [econ]
  "See if the game is won or lost"
  (cond
    (< (:p econ) 5) "win!"
    (> (:p econ) 5) "lose!"
    :else "idk"))

(comment
  ;; test a passing quarter
  (def example-econ (atom start-econ))
  (swap! example-econ pass-quarter 0)
  (reset! example-econ start-econ)
  ;; show 16 quarters in a table
  (do
    (use 'clojure.pprint)
    (reset! example-econ start-econ)
    (print-table (take 16 (repeatedly #(swap! example-econ pass-quarter 0)))))
  ,)


;; include a [] of all econs in order to calculate scores
(defn game
  "The game function waits for a policy (interest rate - i)
  to be applied in response to the state of the economy"
  []
  (loop [econ start-econ]
    (let [current-quarter (:quarter econ)]
      (println "The inflation rate in quarter " current-quarter ": " (:p econ)
               " enter your target i:")
      (if (< current-quarter 3)
        (recur (pass-quarter econ (set-policy)))
        (game-over econ)))))

(comment
  (set-policy)
  (update start-econ :p +)
  (game)
  *print-readably*
  *print-level*
  
  ;; https://eml.berkeley.edu/~dromer/papers/ISMP%20Text%20Graphs%202013.pdf
  ;; https://global.oup.com/uk/orc/busecon/economics/carlin_soskice/student/excelsimulator/
  ;;; https://voxeu.org/sites/default/files/file/DP7979.pdf
  )
