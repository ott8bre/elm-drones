import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window
import Array exposing (..)

import Point exposing (..)
import Drone exposing (..)

(gameWidth,gameHeight) = (600,400)

--SCENARIO

weights = [4,2,7,3,1]
droneMaxLoad = 12

-- MODEL

type State = Play | Pause

type alias Game =
  {  state : State
  , turn : Int
  , drone : Drone
  , queue : List (Drone -> Drone)
  }

type alias Input =
  { space : Bool
  , delta : Time
  }

model : Game
model =
  { state = Pause
  , turn = 0
  , drone = initDrone 0 0
  , queue = []
  }

load : Point -> Int -> Int -> Drone -> List( Drone -> Drone )
load p t n d =
  let
    x = round <| distance d.position p
    f = List.repeat x (flyTo p)
  in
    f ++ [take t n]

deliver : Point -> Int -> Int -> Drone -> List( Drone -> Drone )
deliver p t n d =
  let
    x = round <| distance d.position p
    f = List.repeat x (flyTo p)
  in
    f ++ [drop t n]

initDrone = newDrone droneMaxLoad <| List.length weights

totalWeight : Drone -> Int
totalWeight a =
  List.foldr (+) 0 <| List.map2 (*) weights <| toList a.products

--UPDATE

update : Input -> Game -> Game
update {space} ({state, turn, drone, queue} as game) =
  let
    state' = if space then Play else Pause
    newTurn = if state' == Play then 1+turn else turn
    newQueue' = if state' == Play && queue == [] then
        if items drone == 0 then
          queue ++ load (Point 20 5) 1 5 drone
        else
          queue ++ deliver (Point 5 20) 1 5 drone
      else
        queue
    newQueue = if state' == Play then List.drop 1 newQueue' else newQueue'

    cmd = if state' == Play then List.head newQueue' else Nothing
    newDrone =
      case cmd of
        Nothing -> drone
        Just z -> Debug.log "next" (z drone)

  in
    { game |
      state = state',
      turn = newTurn,
      drone = newDrone,
      queue = newQueue
    }

-- VIEW

view : (Int,Int) -> Game -> Element
view (w,h) game =
  let
    scores =
      txt identity ("Turn " ++ toString game.turn)
  in
    container w h middle <|
    collage gameWidth gameHeight
     [ rect gameWidth gameHeight
          |> filled pongGreen
      , toForm scores
          |> move (0, gameHeight/2 - 40)
      , toForm (if game.state == Play then spacer 1 1 else txt identity msg)
          |> move (0, 40 - gameHeight/2)
      , make 5 white game.drone.position
      ]

pongGreen =
  rgb 60 60 60

pongRed =
  rgb 100 60 60

textGreen =
  rgb 160 200 160


txt f string =
  Text.fromString string
    |> Text.color textGreen
    |> Text.monospace
    |> f
    |> leftAligned


msg = "SPACE to step"

make r c obj =
  oval r r
    |> filled c
    |> move (obj.x, obj.y)

--SIGNALS

--main =
--  show model


main =
  Signal.map2 view Window.dimensions gameState


gameState : Signal Game
gameState =
  Signal.foldp update model input


delta =
  Signal.map inSeconds (fps 10)

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map2 Input
      Keyboard.space
      delta
