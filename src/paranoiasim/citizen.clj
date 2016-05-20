(ns paranoiasim.citizen
  (:require
   [taoensso.timbre :as log]
   )
  )

(log/set-level! :info)

(def citizenSkills
  "List of keywords for citizen skills"
  [:hardware :software :wetware :management :violence :subterfuge])

(def personalityTraits
  "List of personality traits for citizens"
  [:ambition])

;; TODO update actions
;; terminationDiff is how treasonous above their commendations a clone needs to be found before termination. Smaller amounts may result in other disciplinary actions
(def clearances
  "Map of citizen clearances, with monthly salaries and advancement tracks"
  {:IR {:salary 100 :actions 5 :promotion :R :terminationDiff 20}
   :R {:salary 1000 :actions 10 :promotion :O :demotion :IR :percentage 0.1 :terminationDiff 25}
   :O {:salary 2000 :actions 20 :promotion :Y :demotion :R :percentage 0.06 :terminationDiff 30}
   :Y {:salary 3000 :actions 30 :promotion :G :demotion :O :percentage 0.01 :terminationDiff 40}
   :G {:salary 10000 :actions 40 :promotion :B :demotion :Y :percentage 0.01 :terminationDiff 50}
   :B {:salary 40000 :actions 50 :promotion :I :demotion :G :percentage 0.001 :terminationDiff 60}
   :I {:salary 100000 :actions 60 :promotion :V :demotion :B :percentage 0.0001 :terminationDiff 70}
   :V {:salary 600000 :actions 70 :promotion :U :demotion :I :percentage 0.00001 :terminationDiff 80}
   :U {:salary 100000000 :actions 80 :demotion :V :percentage 0.000001 :terminationDiff 100} ;; Yes, that is 100mil. ULTRAVIOLETs are freaking rich
   })

;; TODO
(defn- create-random-name
  "Creates a random name"
  []
  "testName")

(defn create-citizen
  "Creates a new citizen with a random name, skills, and personality, with the assigned ID."
  [id]
  {id
   {:id id
    ;; Citizen's name
    :name (create-random-name)
    ;; Citizens skills
    :skills (reduce merge (map (fn [kw] {kw (inc (rand-int 20))})
                               citizenSkills))
    ;; Citizens personality, affects decisions
    :personality (reduce merge (map (fn [kw] {kw (inc (rand-int 20))})
                                    personalityTraits))
    ;; Current clearance
    :clearance :IR
    ;; Current relationships of the citizen
    :relationships #{}
    ;; Clone's current number, starts at 1
    :cloneNum 1
    ;; The clone's current amount of money in their bank
    :bank (-> clearances (:IR) (:salary))
    ;; How treasonous the clone has been. Termination happens when treason - commendations > terminationDiff
    :treason 0
    :commendations 0
    }
   })

(defn citizen-promote
  "Promotes a citizen, and adds the new monthly salary as a bonus"
  [citizen]
  (let [newClearance (-> clearances ((:clearance citizen)) (:promotion))]
    (if newClearance
      (-> citizen
          (assoc-in [:clearance] newClearance)
          (update-in [:bank] + (-> clearances (newClearance) (:salary)))
          )
      )
    )
  )

(defn citizen-demote
  "Demotes a citizen. Does not modify their bank balance"
  [citizen]
  (let [newClearance (-> clearances ((:clearance citizen)) (:demotion))]
    (if newClearance
      (-> citizen
          (assoc-in [:clearance newClearance])
          )
      )
    )
  )

(defn create-sector
  "Creates a sector with a population and starting id"
  [population startId]
  {:nextId (+ startId population)
   :citizens (reduce merge (map create-citizen
                         (range startId
                                (+ startId population))))
   }
  )

(defn citizen-get-all-as-map
  "Gets the entire list of citizens as an id map"
  [sector]
  (-> sector (:citizens)))

(defn citizen-get-all-as-list
  "Gets the entire list of citizens from a sector"
  [sector]
  (-> sector (:citizens) (vals)))

(defn citizens-find-best-promotable
  "Checks a list of citizens to promote, returns the id of the best citizen to promote. The best citizen is chosen purely by commendations, not treason."
  [citizens]
  (if (and citizens
           (> (count citizens) 0))
    (:id (reduce (fn [f s] (if (> (:commendations s) (:commendations f)) s f) ) citizens))
    nil
    )
  )

(defn citizens-get-all-by-clearance
  "Gets a list of all citizens at a specific clearance"
  [sector clearance]
  (filter (fn [c] (= clearance (:clearance c))) (citizen-get-all-as-list sector))
  )

(defn citizens-promote-best-by-clearance
  "Promotes the best citizen at the given clearance"
  [sector clearance]
  (let [promId (citizens-find-best-promotable (citizens-get-all-by-clearance sector clearance))]
    (if promId
      (do
        (log/trace "Promoting citizen" promId "from" clearance)
        (update-in sector [:citizens promId] citizen-promote)
        )
      (do
        (log/trace "No citizens of clearance" clearance "to promote")
        sector
        )
      )
    )
  )

(defn citizens-check-and-promote
  "Checks the specified clearance for total numbers, and promotes a citizen to that clearance if there's not enough"
  [clearance sector]
  (log/trace "Clearance:" clearance)
  (let [per (get-in clearances [clearance :percentage])
        citCount (count (citizens-get-all-by-clearance sector clearance))]
    (if (and per (get-in clearances [clearance :demotion]))
      (if (< citCount (* per (count (citizen-get-all-as-map sector))))
        (do
          (log/trace "Found and promoting a citizen to" clearance)
          (citizens-promote-best-by-clearance sector (get-in clearances [clearance :demotion]))
        )
        (do
          (log/trace "Did not need to promote a citizen to" clearance)
          sector
          )
        )
      (do
        (log/trace "Clearance doesn't require quota at" clearance)
        sector
        )
      )
    )
  )

(defn sector-balance-clearances
  "Checks if enough citizens are in stations of power, and promotes one per clearance as required.
  Does not demote. May shoot a citizen from IR to U if required"
  [sector]
  ((apply comp (map partial
                    (repeat citizens-check-and-promote)
                    (keys clearances)))
   sector
   )
  )

;; Debugging scripts
(def testSec (create-sector 5 1))
(def sector testSec)
(citizens-find-best-promotable (-> testSec (:citizens) (vals)))
(citizens-get-all-by-clearance testSec :IR)
(citizens-check-and-promote :U testSec)
(sector-balance-clearances testSec)
((apply comp (repeat 5 sector-balance-clearances)) testSec)
