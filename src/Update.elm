module Update exposing (Msg(..), update, init)

import Time exposing (..)
import Window exposing (Size)
import Task
import Array exposing (..)

import Model exposing (Game, State(Play, Pause, End))

import Env exposing (..)
import Point exposing (..)
import Packet exposing (Packet)
import Drone exposing (Drone, Msg(Take,Drop))


type Msg
  = Resize Size
  | Tick Time
  | TogglePlay
  | NoOp

init : (Game, Cmd Msg)
init =
  (initialModel, Task.perform (\_ -> NoOp) Resize (Window.size))

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
            roundPoint = \p -> (round p.x, round p.y)
            xxx = Debug.log "drones" {- <| List.map ( .position>>roundPoint ) -} model.drones
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
      let
        takeDrop packet (drone, rest) = 
          if Drone.canLoad packet drone
          then ( drone |> Drone.enqueue (Take packet) |> Drone.enqueue (Drop packet) , rest)
          else ( drone , packet :: rest)
                   
        (newDrone, others) = List.foldr takeDrop (drone,[]) ps
      in
        (others, Drone.update newDrone :: ds)
  in
    List.foldr func (packets, []) drones

initialModel : Game
initialModel =
  { state = Pause
  , turn = 0
  , size = Size 0 0
  , drones = List.repeat Env.numberOfDrones initDrone
  , packets = List.map orderToPackets orders |> List.concat
  , products = List.map waresToProducts warehouses
  }

orderToPackets : Target -> List Packet
orderToPackets order =
  let
    start = List.head warehouses |> Maybe.map .address |> Maybe.withDefault Point.origin
    f x y = Packet.init x y start order.address
    packs = Array.indexedMap f order.items
  in
    Array.toList packs 
    |> List.filter (\p -> p.copies /= 0)


waresToProducts : Target -> Int
waresToProducts ware =
  Array.toList ware.items |> List.sum

initDrone : Drone
initDrone = 
  List.head warehouses
  |> Maybe.map .address
  |> Maybe.withDefault Point.origin
  |> Drone.init Env.droneMaxLoad