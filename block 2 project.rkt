#lang racket/gui
;GUI starts here
(define main-window (new frame%
                   [label "Travel"]
                   [width 300] [height 300]
                   )
  )


;the textfield to input the starting station and destination station
(define beginning-box (new text-field%
                        [parent main-window]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [label "Start Station"]
                        )
  )
(define destination-box (new text-field%
                         [parent main-window]
                         [vert-margin 10]
                         [horiz-margin 10]
                         [label "End Station"]
                         )
  )

;this clears the textfield
(define clear-text (new button%
                        [parent main-window]
                        [label "Clear"]
                        [callback (λ (o e)
                                    (send beginning-box set-value "")
                                    (send destination-box set-value "")
                                    )
                                  ]
                        )
  )

;this button when pressed should display the route to take

(define start-plan (new button%
                  [parent main-window]
                  [label "Create Plan"]
                  [callback (λ (button event)
                              (send route-msg set-label (new-station (send beginning-box get-value) (send destination-box get-value)))
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





;list of stations (linear)
(define stations '("A" "B" "C" "D" "E" "F" "G" "H"))


;this function creates a new list of the stations depending on the start and end specified
(define new-station (λ (start end)
                      (define start-index (index-of stations start))
                      (define end-index (index-of stations end))
                      (cond
                        ((> start-index end-index)
                         (define temp-list1 (member end stations))
                         (define temp-list2 (member start (reverse temp-list1)))
                         (string-join temp-list2 " ")
                         )
                        ((< start-index end-index)
                         (define temp-list1 (member start stations))
                         (define temp-list2 (member end (reverse temp-list1)))
                         (string-join (reverse temp-list2) " ")
                         )
                        ((= start-index end-index)
                         "You're already here :DDDDDDDDDDDDD"
                         )
                        )
                      )
  )





;shows GUI onscreen
(send main-window show #t)