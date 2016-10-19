module Drone exposing
    ( Drone , Msg(Take,Drop)
    , init , update
    , load , enqueue
    , weight , isEmpty
    )

import Point exposing (Point)
import Packet exposing (Packet)

type Msg 
  = Take Packet
  | Drop Packet
  | NoOp

type Status 
  = Idle      -- 
  | Loading   -- means fly to & load
  | Unloading -- means fly to & unload

type alias Drone =
  { schedule: List Msg
  , packets: List Packet
  , position: Point
  , maxLoad: Int
  }

init : Int -> Point -> Drone
init load point =
  { schedule = []
  , packets = []
  , position = point
  , maxLoad = load
  }

enqueue : Msg -> Drone -> Drone
enqueue msg drone = 
  { drone | schedule = drone.schedule ++ [msg] }

load : Packet -> Drone -> Drone
load packet model =
  { model | packets = List.append model.packets [packet] }
  

distance : Point -> Point -> Int
distance p q =
    Point.distance p q |> floor


stepTo : Point -> Point -> Point
stepTo src dest =
  let
    d = distance src dest
    p = if d == 0 then 1 else 1 / toFloat d
  in
    { src
    | x = (1-p) * src.x + p * dest.x
    , y = (1-p) * src.y + p * dest.y
    }

currentMsg : Drone -> Msg
currentMsg drone =
  drone.schedule |> List.head |> Maybe.withDefault NoOp

update : Drone -> Drone
update model =
--  case model.status of 
--    _ ->
      let
        msg = model.schedule |> List.head |> Maybe.withDefault NoOp

        newPackets = 
          if model.position == newPos then
            List.tail model.packets
            |> Maybe.withDefault []
          else
            model.packets 
     
        newPos = 
          List.head model.packets 
          |> Maybe.map .recipient
          |> Maybe.withDefault model.position
          |> stepTo model.position 
        
      in
        { model 
        | position = newPos
        , packets = newPackets 
        }


isEmpty : Drone -> Bool
isEmpty model =
  List.isEmpty model.packets

weight : Drone -> Int
weight model =
  model.packets
  |> List.map Packet.weight
  |> List.foldr (+) 0

{-

items : Drone -> Int
items d =
  foldr (+) 0 d.products

take : Int -> Int -> Drone -> Drone
take t n d =
  let
    x = get t d.products
    newP = case x of
      Just z ->
        set t (z+n) d.products
      Nothing ->
        d.products
  in
    { d | products = newP}

drop : Int -> Int -> Drone -> Drone
drop t n d =
  let
    x = get t d.products
    newP = case x of
      Just z ->
        set t (z-n) d.products
      Nothing ->
        d.products
  in
    { d | products = newP}

distance : Point -> Point -> Int
distance p q =
    Point.distance p q |> floor

move1 : Point -> Point -> Point
move1 src dest =
  let
    d = distance src dest
    p = if d == 0 then 1 else 1 / toFloat d
  in
    { src
    | x = (1-p) * src.x + p * dest.x
    , y = (1-p) * src.y + p * dest.y
    }

flyTo : Point -> Drone -> Drone
flyTo dest drone =
  { drone | position = move1 drone.position dest}

-}
