;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable declarations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

globals
[
  grid-x-inc  ;; the amount of patches in between two roads in the x direction
  grid-y-inc  ;; the amount of patches in between two roads in the y direction
  acceleration  ;; the constant that controls how much a car speeds up or slows down by if it is to accelerate or decelerate
  phase  ;; keeps track of the phase
  num-cars-stopped  ;; the number of cars that are stopped during a single pass thru the go procedure
  old-display-which-metric  ;; holds the value of display-which-metric for the last time through the go procedure
  ;; patch agentsets
  intersections  ;; agentset containing the patches that are intersections
  roads  ;; agentset containing the patches that are roads
  successful-rides
  unsuccessful-rides
  total-rides
  person-miles-per-vehicle-mile
  speed-limit
  simulation-speed
  ticks-per-cycle
  congestion-level
  ]
;; Driver functions
breed [drivers a-driver]
drivers-own
[  speed  ;; the speed of the turtle
  up-car?  ;; this will be true if the turtle moves downwards and false if it moves to the right
  wait-time  ;; the amount of time since the last time a turtle has moved
  passenger_amount ;;possible use when assigning passengers to single drivers
  d-valuation
  target
  is-solo?]



;; Passenger functions
breed [passengers a-passenger]
passengers-own
[
  speed  ;; the speed of the turtle
  ;up-car?  ;; this will be true if the turtle moves downwards and false if it moves to the right
  ;wait-time
  ;is-passenger?
  p-valuation
  time-on-patch
  max-time-on-patch
  ]

patches-own
[  intersection?  ;; this is true if the patch is at the intersection of two roads
  is-target? ;;target for house placement
  is-endpoint? ;;end point of the road where the trip ends
  ]




;;;;;;;;;;;;;;;;;;;;;
;; Setup Functions ;;
;;;;;;;;;;;;;;;;;;;;;

to startup
  setup
end

;; Initialize the display by giving the global and patch variables initial values.
;; Create num-cars of turtles if there are enough road patches for one turtle to be created per road patch.
;; Setup the plots
;; All code in setup is done if full-setup? is true.  If it is false, then it doesn't clear the information
;; about the users; users still retain the same light that they had before.
;; "setup false" is done by the re-run button.
to setup
  clear-output
  clear-turtles
  clear-all-plots

  let full-setup? ((grid-x-inc != (world-width / grid-size-x))
      or (grid-y-inc != (world-height / grid-size-y)))

  setup-globals
    clear-patches
    setup-patches
    setup-intersections
    name-target
    name-endpoint

    set-default-shape drivers "car"
set-default-shape passengers "person"

  ;if (number > count roads) [ user-message (word  "There are too many cars for the amount of road.  ")  stop ]

  ;; Now create the turtles and have each created turtle call the functions setup-cars and set-car-color
  create-passengers number [setup-passengers]
  ask drivers ;; give the drivers an initial speed
  [    set-car-speed        ]

  reset-ticks
end


;; Initialize the global variables to appropriate values
to setup-globals
  set phase 0
  set num-cars-stopped 0
  set grid-x-inc world-width / grid-size-x ; = 18/3 --> 6
  set grid-y-inc world-height / grid-size-y ; = 18/3
  set speed-limit 1.0
  set simulation-speed 10.0
  set ticks-per-cycle 20
  ;; don't make acceleration 0.1 since we could get a rounding error and end up on a patch boundary
  set acceleration 0.099
  set successful-rides 0
  set unsuccessful-rides 0
  set total-rides 0
  set person-miles-per-vehicle-mile 0
  set congestion-level 0
end

;; Make the patches have appropriate colors, setup the roads and intersections agentsets,
;; and initialize the traffic lights to one setting
to setup-patches
  ;; initialize the patch-own variables and color the patches to a base-color
  ask patches
  [ set intersection? false
    set pcolor brown + 3  ]

  ;; initialize the global variables that hold patch agentsets
  set roads patches with [ (floor ((pxcor + max-pxcor - floor(grid-x-inc - 1)) mod grid-x-inc) = 0) or
                           (floor ((pycor + max-pycor) mod grid-y-inc) = 0) ]
  set intersections roads with [ (floor ((pxcor + max-pxcor - floor(grid-x-inc - 1)) mod grid-x-inc) = 0) and
                                 (floor ((pycor + max-pycor) mod grid-y-inc) = 0) ]

  ask roads  [ set pcolor white ]
end

;; Give the intersections appropriate values for the intersection?, my-row, and my-column
;; patch variables.  Make all the traffic lights start off so that the lights are red
;; horizontally and green vertically.
to setup-intersections
  ask intersections  [  ]
end

;; Initialize the turtle variables to appropriate values and place the turtle on an empty road patch.
to setup-cars  ;; turtle procedure
  set speed 0
  set wait-time 0
  ;assign-drivers

  put-on-empty-road

  ifelse intersection?
  [
    ifelse random 2 = 1
    [ set up-car? true ]
    [ set up-car? false ]
  ]
  [
    ifelse (floor ((pxcor + max-pxcor - floor(grid-x-inc - 1)) mod grid-x-inc) = 0)
    [ set up-car? true ]
    [ set up-car? false ]
  ]

  ifelse up-car?
  [ set heading 180 ]
  [ set heading 90 ]
end

;; Places drivers on green road patches
to put-on-empty-road  ;; turtle procedure
  move-to one-of roads with [pcolor = green]
end
to setup-passengers
  move-to one-of patches with [pcolor = green]
  mobility-valuation
  set time-on-patch 0

  ;set the maximum accepted wait time by each passenger (until he leaves) to a random value of up to the global maximum wait time
  set max-time-on-patch random (max-global-wait-time + 1)
end



;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; receives information from the clients and runs the simulation
to go
  ;; get commands and data from the clients
 ; listen-clients

  every delay
  [
    set num-cars-stopped 0

    ;; set the turtles speed for this time thru the procedure, move them forward their speed,
    ;; record data for plotting, and set the color of the turtles
    ;; to an appropriate color based on their speed
    ask drivers
    [ set-car-speed
      fd speed
      record-data
      set-car-color ]

    ;; update the clock and the phase
    clock-tick
  ]
  update-passenger-wait
  match-on-same-patch
  jump-to-start
  if hide-links = True [ask links [hide-link]]
  update-congestion
  if ticks >= 30000 [stop]
end




;; reports the amount of seconds by which to slow the model down
to-report delay
  ifelse simulation-speed <= 0
  [ report ln (10 / 0.001) ]
  [ report ln (10 / simulation-speed) ]
end


to set-car-speed
  ask one-of drivers [
    ifelse up-car?
     [ set-speed 0 -1
       ;set-solo-speed 0 -1
       ]
    [ set-speed 1 0
      ;set-solo-speed 1 0
      ]]

end

;; set the speed variable of the turtle to an appropriate value (not exceeding the
;; speed limit) based on whether there are turtles on the patch in front of the turtle
to set-speed [delta-x delta-y]  ;; turtle procedure
  let drivers-ahead drivers-on patch-at delta-x delta-y ;change turtles-on to drivers-on
  ;; if there are turtles in front of the turtle, slow down
  ;; otherwise, speed up
  ifelse any? drivers-ahead
  [
    let up-cars?-ahead [up-car?] of drivers-ahead
    ifelse member? up-car? up-cars?-ahead and member? (not up-car?) up-cars?-ahead
    [    ]
    [
      set speed [speed] of one-of drivers-ahead
      slow-down
    ]
  ]
  [ speed-up ]
end


;; decrease the speed of the turtle
to slow-down  ;; turtle procedure
  ifelse speed <= 0  ;;if speed < 0
  [ set speed 0 ]
  [ set speed speed - acceleration ]
end

;; increase the speed of the turtle
to speed-up  ;; turtle procedure
  ifelse speed > speed-limit
  [ set speed speed-limit ]
  [ set speed speed + acceleration ]
end

;; set the color of the turtle to a different color based on how fast the turtle is moving
to set-car-color  ;; turtle procedure
  if breed = drivers [
  ifelse speed < (speed-limit / 2)
  [ set color red ]
  [ set color blue - 2 ]
  ]

end

;; keep track of the number of stopped turtles and the amount of time a turtle has been stopped
;; if its speed is 0
to record-data  ;; turtle procedure
  ifelse speed = 0
  [
    set num-cars-stopped num-cars-stopped + 1
    set wait-time wait-time + 1
  ]
  [ set wait-time 0 ]
end

;; increases the clock by 1 and cycles phase to the next appropriate value
to clock-tick
  tick
  ;; The phase cycles from 0 to ticks-per-cycle, then starts over.
  set phase phase + 1
  if phase mod ticks-per-cycle = 0
  [ set phase 0 ]
end

to hide-or-show-pen [name-of-plot]
  ;ifelse plots-to-display = "All three plots" or plots-to-display = name-of-plot
  ;[
    __plot-pen-show
    ;]
 ; [ __plot-pen-hide ]
end





;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Additions  ;;
;;;;;;;;;;;;;;;;;;;;;;;

;Mobility valuation
to mobility-valuation
  set p-valuation (1 + random-float 9)
end
;;;depending on external effects (government action), the valuation of drivers changes accordingly
to driver-valuation
  if government-action = "subsidy" [set d-valuation ((1 + random-float 9) * (1 - government-action-amount - congestion-level))]
  if government-action = "tax" [set d-valuation ((1 + random-float 9) * (1 + government-action-amount - congestion-level))]
  if government-action = "nothing" [set d-valuation ((1 + random-float 9)* (1 - congestion-level))]
end


;;sets every patch at y=18 or x=-18 which is located on a road to a "target"
to name-target
  ask patches [
    if (pycor = 18) or (pxcor = -18) and (pcolor = white)
  [set is-target? true
    set pcolor green
  ]
  ]
end
;;;sets every patch at y=18 or x=-18 which is located on a road to an "endpoint"
to name-endpoint
  ask patches [
    if count neighbors4 with [pcolor = white] = 3 and (pcolor = white)
  [set is-endpoint? true
    set pcolor blue  ]
  ]
  ask patch 18 -18 [set is-endpoint? true
    set pcolor blue  ]
end

to assign-drivers
  ;;; Depending on d-valuation, make drivers be solo drivers or "regular" drivers who take passengers
  ifelse (d-valuation < 2)
  [set is-solo? True]
  [set is-solo? False]

end


to update-personmiles
  let pmpvm (passenger_amount + 1)
  set person-miles-per-vehicle-mile (person-miles-per-vehicle-mile + pmpvm)
  set total-rides (total-rides + 1)
end
;;drivers reaching end point jump back to random start
to jump-to-start
      ask drivers [ifelse (up-car? = True)
      [if (pcolor = blue) and  (pxcor != 18)[update-personmiles change-to-passenger ask my-links [ask other-end [setup-passengers]]
           ask my-links [die] ]  ] ;for up-car True
      [if (pcolor = blue) and (pycor != -18)[update-personmiles change-to-passenger ask my-links [ask other-end [setup-passengers]]
           ask my-links [die] ]  ] ;for up-car False
      if (pxcor = 18) and (pycor = -18)[update-personmiles change-to-passenger ask my-links [ask other-end [setup-passengers]]
         ask my-links [die] ] ;for the rest
      ]
    ;setup-cars
end
to change-to-passenger
  set breed passengers setup-passengers ;set size 5
end
to change-to-driver
    set breed drivers
    setup-cars
    set-car-color
    record-data
    driver-valuation
    assign-drivers ;;depending on d-valuation, make drivers be solo drivers or "regular" drivers who take passengers
    set-car-speed

end


;;; Passengers who didn't find any driver to take them wait for 100 ticks, then they update their p-valuation (implies that they either are willing to pay
;;; more or just leave and someone else takes their place
to update-passenger-wait
  ask passengers with [pcolor = green]
    [set time-on-patch time-on-patch + 1 ; increase variable for waiting time on patch

     if time-on-patch > (max-time-on-patch) [set unsuccessful-rides unsuccessful-rides + 1 change-to-driver]
      ]
    ask passengers [if time-on-patch > max-time-on-patch  [show "ERROR" wait 100]] ;for debugging
end

to match-on-same-patch
 ask passengers with [ not any? my-links]  [
   let pval p-valuation
   let driver-candidates drivers-here with [(is-solo? = false) and (d-valuation < pval) and (count my-links < 4)]
   if any? drivers-here with [(d-valuation < pval) and (is-solo? = false) and (count my-links < 4)]
      [ create-link-with min-one-of driver-candidates [d-valuation] set successful-rides successful-rides + 1]
                                            ]
 ask passengers with [any? my-links] [setxy 0 0 set time-on-patch 0]
 ask drivers with [any? my-links] [set passenger_amount count my-links]

 ask drivers with [is-solo? = True and any? my-links] [show count my-links]  ;for debugging, if any results show up something went wrong
 ask drivers with [count my-links > 4] [show "ERROR" wait 100] ;for debugging
end

to update-congestion
  set congestion-level ((num-cars-stopped + 0.0001) / (count drivers + 0.0001))
end
@#$#@#$#@
GRAPHICS-WINDOW
282
95
664
498
18
18
10.0541
1
10
1
1
1
0
1
1
1
-18
18
-18
18
1
1
1
ticks
30.0

PLOT
565
500
844
689
Average Wait Time of Cars
Time
Average Wait
0.0
100.0
0.0
5.0
true
false
"" ""
PENS
"default" 1.0 0 -1893860 true "" "hide-or-show-pen  \"Average Wait Time of Cars\" \nif any? drivers [plot mean [wait-time] of drivers]"

PLOT
283
500
562
689
Average Speed of Cars
Time
Average Speed
0.0
100.0
0.0
1.0
true
false
"set-plot-y-range 0 speed-limit" ""
PENS
"default" 1.0 0 -1893860 true "" "hide-or-show-pen \"Average Speed of Cars\" \nif any? drivers [plot  mean [speed] of drivers]"

SLIDER
139
10
278
43
grid-size-y
grid-size-y
1
9
3
1
1
NIL
HORIZONTAL

SLIDER
1
10
139
43
grid-size-x
grid-size-x
1
9
3
1
1
NIL
HORIZONTAL

SLIDER
-1
42
278
75
number
number
0
400
400
1
1
People
HORIZONTAL

PLOT
1
500
280
689
Stopped Cars
Time
Stopped Cars
0.0
100.0
0.0
100.0
true
false
"set-plot-y-range 0 number" ""
PENS
"default" 1.0 0 -1893860 true "" "hide-or-show-pen \"Stopped Cars\"\nplot num-cars-stopped"

BUTTON
200
107
278
150
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
140
108
200
152
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
287
10
384
55
Population
count drivers + count passengers
17
1
11

MONITOR
665
451
828
496
Current avg. speed of cars
precision mean [speed] of drivers 2
17
1
11

MONITOR
443
10
520
55
Passengers
count passengers
17
1
11

MONITOR
385
10
442
55
Drivers
count drivers
17
1
11

MONITOR
521
10
608
55
 is-solo? drivers
count drivers with [is-solo? = True]
17
1
11

MONITOR
664
407
787
452
Current amount of links
count links
17
1
11

MONITOR
157
385
281
430
Total number of drivers
unsuccessful-rides
17
1
11

MONITOR
0
385
156
430
Total number of passengers
successful-rides
17
1
11

SLIDER
1
74
172
107
max-global-wait-time
max-global-wait-time
0
5
5
1
1
NIL
HORIZONTAL

SWITCH
172
74
278
107
hide-links
hide-links
0
1
-1000

CHOOSER
1
107
139
152
government-action
government-action
"nothing" "subsidy" "tax"
0

SLIDER
-1
150
200
183
government-action-amount
government-action-amount
0
1
0
0.1
1
NIL
HORIZONTAL

MONITOR
663
363
753
408
Current PM/VM
precision (mean [(passenger_amount + 1)] of drivers) 4
17
1
11

MONITOR
4
292
128
337
Finished rides of drivers
total-rides ;single rides of drivers \n;(who may have passengers onboard)\n;increases every time a driver \n;reaches a destination
17
1
11

MONITOR
111
232
201
277
PM/VM total
precision (person-miles-per-vehicle-mile / total-rides) 4
17
1
11

MONITOR
662
318
794
363
Current congestion level
precision congestion-level 4
17
1
11

MONITOR
0
232
83
277
Matching rate
precision (successful-rides / (successful-rides + unsuccessful-rides)) 4
17
1
11

MONITOR
123
339
280
384
Total rides
successful-rides + unsuccessful-rides
17
1
11

@#$#@#$#@
## WHAT IS IT?

## HOW TO USE IT

### Quickstart instructions

### Buttons

### Sliders

### Choosers

### Switches

### Plots

STOPPED CARS - displays the number of stopped cars over time
AVERAGE SPEED OF CARS - displays the average speed of cars over time
AVERAGE WAIT TIME OF CARS - displays the average time cars are stopped over time

### Client Information

## THINGS TO NOTICE

## THINGS TO TRY

## EXTENDING THE MODEL

## HOW TO CITE

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

stoplight
false
0
Circle -7500403 true true 30 30 240

swastika
true
0
Line -7500403 true 150 90 150 210
Line -7500403 true 90 150 210 150
Line -7500403 true 150 90 210 90
Line -7500403 true 210 150 210 210
Line -7500403 true 90 150 90 90
Line -7500403 true 150 210 90 210

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3
@#$#@#$#@
need-to-manually-make-preview-for-this-model
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
VIEW
292
10
662
380
0
0
0
1
1
1
1
1
0
1
1
1
-18
18
-18
18

BUTTON
162
68
286
101
Change Light
NIL
NIL
1
T
OBSERVER
NIL
C

SLIDER
162
101
286
134
Phase
Phase
0
99
0
1
1
NIL
HORIZONTAL

TEXTBOX
12
10
154
175
Affect the state of your light by pressing the Change Light button, or changing the value of the Phase slider. The Phase slider controls the point in the cycle at which your light with change, and represents a percentage of the total cycle time.
11
0.0
0

MONITOR
173
10
286
59
Located At:
NIL
3
1

@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
