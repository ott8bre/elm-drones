
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


-- MODEL

(gameWidth,gameHeight) = (600,400)


type State = Play | Pause

type Status = Wait | Load Int Int Int | Deliver Int Int Int

type alias Object a =
  { a | x : Float, y : Float, p: List Int }

type alias Order =
  Object {d: List Int}

type alias Warehouse =
  Object {}

type alias Drone =
  Object {status: Status}

drone : Float -> Float -> Drone
drone a b =
  {x=a, y=b, p=[], status=Wait}

order : Float -> Float -> List Int -> Order
order a b c =
  {x=a, y=b, p=c, d=[]}

warehouse : Float -> Float -> List Int -> Warehouse
warehouse a b c =
  {x=a, y=b, p=c}


type alias Game =
  { state : State
  , turn : Int
  , size : Size
  , weights: List Int
  , drones : List Drone
  , warehouses : List Warehouse
  , orders : List Order
  }


defaultGame : Game
defaultGame =
  { state = Pause
  , turn = 0
  , size = Size 0 0
  , weights = [10,20,30]
  , warehouses = [warehouse 100 0 [1,1,1], warehouse 0 100 [1,2,0]]
  , orders = [order 50 50 [1,0,1], order -40 10 [0,2,0]]
  , drones = [drone 100 0
    --, drone 0 100
    ]
  }

type alias Input =
  { space : Bool
  , dir1 : Int
  , dir2 : Int
  , delta : Time
  }


-- UPDATE

type Msg
  = Resize Size
--  | Player1 Int
--  | Player2 Int
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
      let
        newTurn =
          if model.state == Play then
              1+model.turn
          else
              model.turn

        newDrones =
          if model.state == Play then
            List.map (moveDrone model) model.drones
          else
            model.drones

      in
        { model |
            turn = newTurn,
            drones = newDrones
        }
        
square : number -> number
square a =
  a*a

distance : Object k -> Object h -> Int
distance a b =
  let
    sum =  ( square(a.x-b.x) + square(a.y-b.y) )
  in
    sum |> sqrt |> ceiling

nextP : Game -> Status
nextP g =
  let
    list = g.orders
    sum = List.map(\x->List.sum x.p) list
    d = List.foldr (\x y -> y + if x==0 then 1 else 0) 0 sum
    rest = List.drop d list
    o = List.head rest
    --o.p=[]
  in
    case o of
      Nothing ->
        Wait
      Just z ->
        Load d 1 1


moveDrone : Game -> Drone -> Drone
moveDrone g d =
  let
    w = List.head g.warehouses
    o = List.head g.orders
  in
  case d.status of
    Wait ->
      {d | status = nextP g}
    Load id t n ->
      case w of
        Nothing -> d
        Just z -> moveTo z d
    Deliver id t n ->
      case o of
        Nothing -> d
        Just z -> moveTo z d

moveTo : Object z -> Drone -> Drone
moveTo a d =
  let
    dist = Debug.log "dist" (distance a d)
    p = 1 / toFloat dist

  in
    if dist == 0 then
      if d.status == Load 0 1 1 then
        {d | status = Deliver 0 1 1}
      else
        {d | status = Wait}
    else
      {d | x = p * a.x + (1-p) * d.x, y = p * a.y + (1-p) * d.y}


-- VIEW

view : Game -> Html Msg
view model =
  let
    {width, height} = 
      model.size
    scores =
      txt identity ("Turn " ++ toString model.turn)
    wareShapes =
      List.map (make 5 blue) model.warehouses
    orderShapes =
      List.map (make 3 red) model.orders
    dronesShapes =
      List.map (make 3 white) model.drones
  in
    toHtml <|
    container width height middle <|
    collage gameWidth gameHeight ( List.concat [
     [ rect gameWidth gameHeight
          |> filled pongGreen
      , toForm scores
          |> move (0, gameHeight/2 - 40)
      , toForm (if model.state == Play then spacer 1 1 else txt identity msg)
          |> move (0, 40 - gameHeight/2)
      ], wareShapes, orderShapes, dronesShapes ])


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


msg = "SPACE to start/pause"

make r c obj =
  oval r r
    |> filled c
    |> move (obj.x, obj.y)



keyboardProcessor keyCode =
  case keyCode of
    32 -> TogglePlay
    _ -> NoOp

init =
  (defaultGame, Task.perform (\_ -> NoOp) Resize (Window.size))

main =
  App.program
    { init = init
    , update = \msg m -> update msg m ! []
    , view = view
    , subscriptions =
      (\_ -> Sub.batch
        [ Window.resizes Resize
        , Keyboard.downs keyboardProcessor
        --, Keyboard.downs (keyboardProcessor True)
        --, Keyboard.ups (keyboardProcessor False)
        , AnimationFrame.diffs (Tick<<inSeconds)
        ])
    }

{-

-- SIGNALS (removed in 0.17)

main =
  Signal.map2 view Window.dimensions gameState


gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input


delta =
  Signal.map inSeconds (fps 35)

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map4 Input
      Keyboard.space
      (Signal.map .y Keyboard.wasd)
      (Signal.map .y Keyboard.arrows)
      delta

-}