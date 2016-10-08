
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
--import Array exposing (..)

import Env exposing (..)
import Point exposing (..)
import Drone exposing (..)
import Packet exposing (..)

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
  , drones : List Drone.Model --TODO HIDE THIS!
  }

model : Game
model =
  { state = Pause
  , turn = 0
  , size = Size 0 0
  , drones = List.map initDrone [ Point -202 0, Point 0 153, Point -50 -100, Point 0 0 ] 
  }

{-
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
-}

initDrone : Point -> Drone.Model
initDrone = 
  Drone.init droneMaxLoad -- <| List.length weights

--UPDATE

type Msg
  = Resize Size
  | Tick Time
  | TogglePlay
  | NoOp


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

            newDrones = 
              List.map (\drone ->
                if Drone.isEmpty drone then 
                    let
                      packets = orders
                        |> List.map (.address >> Packet.init 1 3)
                    in
                      List.foldr Drone.load drone packets --WOW !!!
                else 
                  Drone.update drone
              ) model.drones
          in
            { model
            | turn = newTurn
            , drones = newDrones
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
      ] 
      ++ List.map (.address >> make 5 Color.red) orders
      ++ List.map Drone.view model.drones
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

-- MAIN --

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