#lang racket/gui
;GUI starts here
(define main-window (new frame%
                   [label "Travel"]
                   [width 300] [height 300]
                   )
  )


;the textfield to input the starting station and destination station
(define beginning-box (new choice%
                        [parent main-window]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [label "Start Station"]
                        [choices (list "Watthamstow Central" "Blackhorse Road" "Tottenham Hale" "Seven Sisters" "Finsbury Park" "Highbury & Islington"
                                       "King's Cross St Pancras" "Euston" "Warren Street" "Oxford Circus" "Green Park" "Victoria" "Pimlico" "Vauxhall" "Stockwell" "Brixton")]
                        )
  )
(define destination-box (new choice%
                         [parent main-window]
                         [vert-margin 10]
                         [horiz-margin 10]
                         [label "End Station"]
                         [choices (list "Watthamstow Central" "Blackhorse Road" "Tottenham Hale" "Seven Sisters" "Finsbury Park" "Highbury & Islington"
                                        "King's Cross St Pancras" "Euston" "Warren Street" "Oxford Circus" "Green Park" "Victoria" "Pimlico" "Vauxhall" "Stockwell" "Brixton")]
                         )
  )

;this clears the textfield
(define clear-text (new button%
                        [parent main-window]
                        [label "Clear"]
                        [callback (λ (o e)
                                    (send beginning-box set-string-selection "A")
                                    (send destination-box set-string-selection "A")
                                    )
                                  ]
                        )
  )

;this button when pressed should display the route to take

(define start-plan (new button%
                  [parent main-window]
                  [label "Create Plan"]
                  [callback (λ (button event)
                              (send route-msg set-label (string-join(FindRoute (send beginning-box get-string-selection) (send destination-box get-string-selection))))
                              )
                            ]
                  )
  )

(define msg (new message%
                 [parent main-window]
                 [label "Route to take:"]
                 )
  )

(define route-msg (new message%
                       [parent main-window]
                       [label "No route generated yet..."]
                       )
  )




;find route function here

(struct station (name)  #:mutable)
(struct connection (station1 station2 distance) )



(define WatthamstowCentral (station "Watthamstow Central" ))
(define BlackhorseRoad (station "Blackhorse Road" ))
(define TottenhamHale (station "Tottenham Hale" ))
(define SevenSisters (station "Seven Sisters" ))
(define FinsburyPark (station "Finsbury Park" ))
(define Highbury&Islington (station "Highbury & Islington" ))
(define KingsCrossStPancras (station "King's Cross St Pancras" ))
(define Euston (station "Euston" ))
(define WarrenStreet (station "Warren Street" ))
(define OxfordCircus (station "Oxford Circus" ))
(define GreenPark (station "Green Park" ))
(define Victoria (station "Victoria" ))
(define Pimlico (station "Pimlico" ))
(define Vauxhall (station "Vauxhall" ))
(define Stockwell (station "Stockwell" ))
(define Brixton (station "Brixton" ))

(define newSetFirst (λ (Set) (cond ((set-empty? Set) "Set is Empty")
                                   (else (set-first Set)))))

(define ListToConnections (λ (List)
                            (for/list ([i (in-range (-(length List) 1))] #:when (even? i))
                                      
                              (connection (list-ref List i)  (list-ref List (+ i 2)) (list-ref List (+ i 1))))))



(define GetNames (λ (Stations) (for/list( [i Stations] ) (station-name i))))


(define ToStation {λ (name)
                    (define tempStation null)
                    (newSetFirst (for/set( [i Route] #:when  (or (equal? (station-name (connection-station1 i)) name)
                                                                 (equal? (station-name (connection-station2 i)) name)))
                                                                     
                                   (cond

                                     ((equal? (station-name (connection-station1 i)) name)
                                      (connection-station1 i))

                                     ((equal? (station-name (connection-station2 i)) name)
                                      (connection-station2 i)))))})
                                                               

(define Remove (λ (List element) (for/list( [i List] 
                                   #:when (not (equal? i element)))

                                   i)))

(define ConnectionToStation (λ (List)
                              (cons (station-name (connection-station2 (first List)))
                                    (for/list( [i List])
                                      (station-name (connection-station1 i))))))


















                                        
                                                 



(define Route (ListToConnections
               (list  WatthamstowCentral 3 BlackhorseRoad 1 TottenhamHale 2 SevenSisters 4 FinsburyPark 6 Highbury&Islington 1 KingsCrossStPancras 7 Euston 1 WarrenStreet 8 OxfordCircus 3 GreenPark 9 Victoria 3 Pimlico 1 Vauxhall 2 Stockwell 5 Brixton )))








(define GetRoute (λ (Current End count newRoute route distance)
                   (cond
                     ((string? Current)  "Starting Station Is Not Valid")
                     ((string? End)  " Destination Is Not Valid")
                     (else 
                      (cond
                        ((> (length route) count) (cond
                                                    
                                                    ((equal? Current (connection-station1 (list-ref route count)))
                                                     (cond
                                                       ((equal? End (connection-station2 (list-ref route count)))
                                                        
                                                        (cons (+ distance (connection-distance(list-ref route count))) (reverse (ConnectionToStation (cons  (list-ref route count)  newRoute)))))
                                                       
                                                                                                                    
                                                       (else
                                                                                                                     
                                                        (GetRoute (connection-station2 (list-ref route count)) End 0
                                                                  (cons  (list-ref route count) newRoute)
                                                                  ( Remove route  (list-ref route count))
                                                                  (+ distance (connection-distance(list-ref route count)))))))
                                                    
                                                    (else (GetRoute Current End (+ count 1) newRoute route distance))))
                        
                        ((> (length newRoute) 0) (GetRoute (connection-station1 (first newRoute)) End 0
                                                           (Remove newRoute (first newRoute))
                                                           (Remove route (first newRoute) )
                                                           (- distance (connection-distance  (first newRoute)))))
                        (else "Route can't be found"))))))






(define FindRoute (λ (current  end)
                    (cond
                      ((number? (first (GetRoute (ToStation current) (ToStation end) 0 '() Route 0)))
                       (list-update (GetRoute (ToStation current) (ToStation end) 0 '() Route 0) 0 number->string)
                       )
                      )
                    )
  )



;shows GUI onscreen
(send main-window show #t)
