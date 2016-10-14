
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

import Env exposing (..)
import Point exposing (..)
import Drone exposing (Drone)

-- MODEL

type State 
  = Play 
  | Pause
  | End

type alias Game =
  { state : State
  , turn : Int
  , size : Size
  , drones : List Drone
  , packets : List Packet
  , products : List Int
  }

initialModel : Game
initialModel =
  { state = Pause
  , turn = 0
  , size = Size 0 0
  , drones = List.map initDrone 
    [ Point -202 0
    , Point 0 153
    , Point -50 -100
    , Point 0 0 
    ] 
  , packets = List.map orderToPackets orders |> List.concat
  , products = List.map waresToProducts warehouses
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

initDrone : Point -> Drone
initDrone = 
  Drone.init Env.droneMaxLoad

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
            _ -> Play
        
      in 
        if model.state == End then { initialModel | size = model.size } else { model | state = newState }

    Tick delta -> 
      case model.state of
        Play ->
          let
            xxx = Debug.log "packets" <| List.length model.packets
            terminated = List.isEmpty model.packets && List.all Drone.isEmpty model.drones
            newState =
              case terminated of
                True -> End
                _ -> model.state

            newTurn =
              1+model.turn

            (newPackets, newDrones) = 
              updatePacketsDrones model.packets model.drones
              
          in
            { model
            | state = newState
            , turn = newTurn
            , drones = newDrones
            , packets = newPackets
            }

        _ ->
          model



updatePacketsDrones : List Packet -> List Drone -> (List Packet , List Drone)
updatePacketsDrones packets drones =
  let 
    func drone (ps, ds) =
      case (ps, Drone.isEmpty drone) of
        (x::rest, True) ->
          (rest, Drone.load x drone :: ds)

        _ ->
          (ps, Drone.update drone :: ds)
  in
    List.foldr func (packets, []) drones

-- VIEW

view : Game -> Html Msg
view model =
  let
    {width, height} = 
      model.size
    scores =
      case model.state of
        Play ->
          spacer 1 1
        _ ->
          txt identity ("Turn " ++ toString model.turn)
    info =   
      case model.state of
        Play ->
          spacer 1 1
        _ ->
          txt identity "press SPACE to start/pause"
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
      ++ List.map (.address >> make 5 Color.blue) warehouses
      ++ List.map (.address >> make 5 Color.red) orders
      ++ List.map Drone.view model.drones
      ++ (List.indexedMap (drawProducts (-gameWidth/2) (-gameHeight/2) ) model.products |> List.concat)
      ++ List.indexedMap (drawPackets (gameWidth/2) (gameHeight/2) ) model.packets
    )

drawProducts : Float -> Float -> Int -> Int -> List Form
drawProducts x y i size =
  let
    f = \h -> square Color.green (x + 5*h, y + 5*k)
    range = [1..size]
    k = 1 + i
  in
    List.map f range
  

drawPackets : Float -> Float -> Int -> Packet -> Form
drawPackets x y i order =
  let
    k = 1 + i
  in
    square Color.brown (x - 5*k, y - 5)


square : Color -> (Float,Float) -> Form
square color (x,y) =
    rect 4 4
      |> filled color
      |> move (x,y)


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


--msg = "press SPACE to start/pause"

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
  (initialModel, Task.perform (\_ -> NoOp) Resize (Window.size))

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