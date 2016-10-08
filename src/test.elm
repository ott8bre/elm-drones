
import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window exposing (Size)
import AnimationFrame
import Html exposing (..)
import Html.App as App
import Task
import Array exposing (..)

import Point exposing (..)
import Drone exposing (..)

(gameWidth,gameHeight) = (600,400)
--(gameWidth,gameHeight) = (900,600)


--SCENARIO

weights = [4,2,7,3,1]
droneMaxLoad = 12

-- MODEL

type State 
  = Play 
  | Pause

type alias Game =
  { state : State
  , turn : Int
  , size : Size
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
  , size = Size 0 0
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

type Msg
  = Resize Size
--  | Player1 Int
--  | Player2 Int
  | Tick Time
  | TogglePlay
  | NoOp

{- EXPERIMENTAL -}
targets : List Point
targets = 
  [ Point 0 (gameHeight/4)
  , Point (gameWidth/4) 0
  , Point 0 -(gameHeight/4)
  , Point -(gameWidth/4) 0
  ]

update : Msg -> Game -> Game
update msg model =
  case msg of
    NoOp -> 
      model

    Resize size -> 
      { model | size = size }
    
    TogglePlay ->
      let
        newState =
          case model.state of
            Play -> Pause
            Pause -> Play
      in 
        { model | state = newState }

    Tick delta -> 
      case model.state of
        Play ->
          let
            newTurn =
              1+model.turn

            newQueue' =
              case model.queue of
                [] ->
                  let
                    rand = model.turn % List.length targets 
                    point = targets 
                      |> List.drop rand 
                      |> List.head
                      |> Maybe.withDefault Point.origin
                    action = case items model.drone of
                      0 -> load                        
                      _ -> deliver
                  in
                      model.queue ++ action point 1 5 model.drone

                _ ->
                  model.queue

            newQueue = 
              List.drop 1 newQueue' 

            newDrone =
              case List.head newQueue' of
                Nothing -> 
                  model.drone
                
                Just z -> 
                  Debug.log "next" (z model.drone)

          in
            { model
            | turn = newTurn
            , drone = newDrone
            , queue = newQueue
            }

        _ ->
          model

-- VIEW

view : Game -> Html Msg
view model =
  let
    {width, height} = 
      model.size
    scores =
      case model.state of
        Pause ->
          txt identity ("Turn " ++ toString model.turn)
        _ ->
          spacer 1 1
    info =   
      case model.state of
        Pause ->
          txt identity msg
        _ ->
          spacer 1 1
  in
    toHtml <|
    container width height middle <|
    collage gameWidth gameHeight (
      [ rect gameWidth gameHeight
          |> filled backgroundColor
      , toForm scores
          |> move (0, gameHeight/2 - 40)
      , toForm info
          |> move (0, 40 - gameHeight/2)
      , make 5 white model.drone.position
      ]
    )

backgroundColor =
  rgb 10 10 10

wareColor =
  blue

orderColor =
  red

droneColor =
  white

textColor =
  rgb 160 200 160


txt f string =
  Text.fromString string
    |> Text.color textColor
    |> Text.monospace
    |> f
    |> leftAligned


msg = "press SPACE to start/pause"

make r c obj =
  oval r r
    |> filled c
    |> move (obj.x, obj.y)



keyboardProcessor keyCode =
  case keyCode of
    32 -> TogglePlay
    _ -> NoOp

init =
  (model, Task.perform (\_ -> NoOp) Resize (Window.size))

main =
  App.program
    { init = init
    , update = \msg m -> update msg m ! []
    , view = view
    , subscriptions =
      (\_ -> Sub.batch
        [ Window.resizes Resize
        , Keyboard.ups keyboardProcessor
        --, Keyboard.downs (keyboardProcessor True)
        --, Keyboard.ups (keyboardProcessor False)
        , AnimationFrame.diffs (Tick<<inSeconds)
        ])
    }