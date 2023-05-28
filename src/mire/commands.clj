(ns mire.commands
  (:use [mire.rooms :only [rooms room-contains? room-contains-gold? room-contains-loot?]]
        [mire.player :as player])
  (:use [clojure.string :only [join]]))

(defn- move-between-refs
  "Move one instance of obj between from and to. Must call in a transaction."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

(defn- move-delete
  [obj from]
  (alter from disj obj))

;; Command functions

(defn look
  "Get a description of the surrounding environs and its contents."
  []
  (str (:desc @*current-room*)
       "\r\nExits: " (keys @(:exits @*current-room*)) "\r\n"
       (join "\r\n" (map #(str % " находится здесь.\r\n")
                           @(:items @*current-room*)))
       (join "\r\n" (map #(str "Игрок " % " здесь.\r\n")
                           @(:inhabitants @*current-room*)))
       (join "\r\n" (map #(str % " находится здесь.\r\n")
                           @(:loot @*current-room*)))
       (join (str "Золото " @(:gold @*current-room*) ".\r\n"))
       (join (str "здоровье: " (@health *name*) ".\r\n"))
       (join (str "счёт: " (@score *name*) ".\r\n"))
       (join (str "жизнь: " (@lives *name*) ".\r\n"))
  ))

(defn players
  "Get a list players"
  []
  (str
      (doseq [inhabitant (keys @lives)]
         (println (str inhabitant ":" (@lives inhabitant)))
    )

  ))
(defn move
  "\"♬ Мы должны выбраться из этого места... ♪\" Дайте направление."
  [direction]
  (dosync
   (let [target-name ((:exits @*current-room*) (keyword direction))
         target (@rooms target-name)]
     (if (not= @( :lock target) #{(some @( :lock target) @*inventory*)})
        (if (not= @( :lock target) #{})
           ( str "Заперто! Найдите " @( :lock target) " чтобы пройти " )
        (if target
           (do
             (move-between-refs *name*
                                (:inhabitants @*current-room*)
                                (:inhabitants target))
             (ref-set *current-room* target)
             (look))
        "Вы не можете пойти таким путем."))
    (if target
       (do
         (move-between-refs *name*
                            (:inhabitants @*current-room*)
                            (:inhabitants target))
         (ref-set *current-room* target)
         (look))
    "Вы не можете пойти таким путем.")))))

(defn grab
  "Поднять что то."
  [thing]
  (dosync
    (cond
    (or (= thing "coin") (= thing "treasuregold") (= thing "bagmoney"))
      (if (room-contains-gold? @*current-room* thing)
        (do
          (case thing
            "coin"
            (do (alter *money* inc) (change-points 1))
            "bagmoney"
            (do (alter *money* + 7) (change-points 7))
            "treasuregold"
            (do (alter *money* + 15) (change-points 15))
          )
          (if (= ((keyword thing) @(:gold @*current-room*)) 1)
            (alter (:gold @*current-room*) dissoc (keyword thing))
            (do
              (def temp-gold ((keyword thing) @(:gold @*current-room*)))
              (alter (:gold @*current-room*) dissoc (keyword thing))
              (alter (:gold @*current-room*) assoc (keyword thing) (- temp-gold 1))
            )
          )
          (str " Вы подняли " thing ".")
        )
        (str " Здесь ничего нет " thing " here.")
      )

      (room-contains? @*current-room* thing)
        (case thing
          "arrows" (do
            (.set player/*arrows* (+ (.get player/*arrows*) 5))
            (move-delete (keyword thing) (:items @*current-room*))
            (println "Вы подобрали стрелы.")
            )
            (do
              (move-between-refs (keyword thing)
                                 (:items @*current-room*)
                                 *inventory*)
              (str "Вы подобрали " thing ".")
            )
        )
      :default (str "Здесь нет " thing " here.")
      )
    )
  )

(defn seemoney
  "Ваш кэш"
  []
  (str (join "\r\n" (map #(str "Золота " % " .\r\n") [(str @*money*)])))
)

(defn discard
  "Положите что-нибудь, что вы несете."
  [thing]
  (if (= #{(keyword thing)} @( :lock @*current-room*))
   (str "Здесь нельзя бросать " @( :lock @*current-room*))
  (dosync
   (if (or (= thing "coin") (= thing "treasuregold") (= thing "bagmoney"))
        (case thing
          "coin" (if (> @*money* 0)
                    (do
                      (alter *money* dec)
                      (change-points -1)
                      (if (room-contains-gold? @*current-room* thing)
                        (def temp-gold ((keyword thing) @(:gold @*current-room*)))
                        (def temp-gold 0)
                      )
                      (alter (:gold @*current-room*) assoc (keyword thing) (+ temp-gold 1))
                      (str "Вы выбросили " (keyword thing) ".")
                    )
                    (str "Недостаточно деняк!")
                  )
          "bagmoney" (if (>= @*money* 7)
                        (do
                          (alter *money* - 7)
                          (change-points -7)
                          (if (room-contains-gold? @*current-room* thing)
                            (def temp-gold ((keyword thing) @(:gold @*current-room*)))
                            (def temp-gold 0)
                          )
                          (alter (:gold @*current-room*) assoc (keyword thing) (+ temp-gold 1))
                          (str "Вы выбросили " (keyword thing) ".")
                        )
                        (str "Недостаточно деняк!")
                      )
          "treasuregold" (if (>= @*money* 15)
                        (do
                          (alter *money* - 15)
                          (change-points -15)
                          (if (room-contains-gold? @*current-room* thing)
                            (def temp-gold ((keyword thing) @(:gold @*current-room*)))
                            (def temp-gold 0)
                          )
                          (alter (:gold @*current-room*) assoc (keyword thing) (+ temp-gold 1))
                          (str "Вы выбросили " (keyword thing) ".")
                        )
                        (str "Недостаточно деняк!")
                      )
        )
        (if (carrying? thing)
          (do (move-between-refs (keyword thing)
                                 *inventory*
                                 (:items @*current-room*))
              (str "Вы выбросили " thing ".")
          )
          (str "Вы не несете " thing ".")
        )
      )
    )
  )
)

(defn inventory
  "Посмотрим что у вас есть."
  []
  (str "Вы несете:\r\n"
       (join "\r\n" (seq @*inventory*))
       "\nУ вас есть " (.get player/*arrows*) " стрелы."
  )
)

(defn detect
  "If you have the detector, you can see which room an item is in."
  [item]
  (if (@*inventory* :detector)
    (if-let [room (first (filter #((:items %) (keyword item))
                                 (vals @rooms)))]
      (str item " в " (:name room))
      (str item " нет в комнатах."))
    "You need to be carrying the detector for that."))

(defn say
  "Say something out loud so everyone in the room can hear."
  [& words]
  (let [message (join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @*current-room*) *name*)]
      (binding [*out* (streams inhabitant)]
        (println *name* " : " message)
        (println prompt)))
    (str "Вы сказали " message)))

(defn help
  "Show available commands and what they do."
  []
  (join "\r\n" (map #(str (key %) ": " (:doc (meta (val %))))
                      (dissoc (ns-publics 'mire.commands)
                              'execute 'commands))))

(defn attack
  "Attack other player"
  [target]
  (dosync
    (if (contains? @health target)
      (if (contains? @(:inhabitants @*current-room*) target)
        (do
          (if (not= (@lives target) "dead")

            (do
          (commute health assoc target (- (@health target) damage))
          (if (< (int(@health target)) 1)
           ((commute lives assoc target "dead")
           (println
          (say (str target " убит " *name* "\r\n")))
          (commute score assoc *name* (+ (@score *name*) 25)))
          )

          "Successful attack.")
          "He is dead")
        )
        "No such target in the room."
      )
      "Target doesn't exist."
    )
  )
)

(defn shoot
  "Shoot another player"
  [target]
  (dosync
    (if (player/carrying? :bow)
      (if (> (.get player/*arrows*) 0)
        (if (contains? @health target)
          (if (contains? @(:inhabitants @*current-room*) target)
            (do
              (commute health assoc target (- (@health target) 50))
              (.set player/*arrows* (- (.get player/*arrows*) 1))
              "Great shot!"
            )
            "No such target in the room."
          )
          "Target doesn't exist."
        )
        "You don't have arrows."
      )
      "You don't have a bow."
    )
  )
)

(defn buy
		"Buy loot from any place"
		[loot]
		(dosync
    (if (or (= loot "sword"))
        			(do
			          (case loot
			            "sword"
			            (if (> @*money* 7)
			            		(do
			            		(move-between-refs (keyword loot)
			                             		(:items @*current-room*)
			                             		*inventory*)
			            		(alter *money* - 7)
			           			(str "Вы купили " loot ".")
			           			)
			      						)
      							)
											)
					(str "Нет " loot " в магазине.")
			)
	)
)

;; Command data
(defn deadplayer
  []
  (str "Вы на небесах \r\n"
  "Ваш счет:" (@score *name*) "\r\n"
  ))

(def commands
              {"move" move,
               "north" (fn [] (move :north)),
               "south" (fn [] (move :south)),
               "east" (fn [] (move :east)),
               "west" (fn [] (move :west)),
               "grab" grab
               "seemoney" seemoney
               "discard" discard
               "inventory" inventory
               "detect" detect
               "look" look
               "say" say
               "players" players
               "help" help
               "attack" attack
               "buy" buy
               "deadplayer" deadplayer
               "shoot" shoot
               })

;; Command handling

(defn execute
  "Execute a command that is passed to us."
  [input]
  (try (let [[command & args] (.split input " +")]
         (apply (commands command) args))
       (catch Exception e
         (.printStackTrace e (new java.io.PrintWriter *err*))
         "Вы не можете этого сделать!")))
