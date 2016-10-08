module Drone exposing
    ( Model
    , init , update
    , load
    , view , weight , isEmpty
    )

import Color
import Collage exposing (..)

import Point exposing (Point)
import Packet exposing (Packet)

type Status 
  = Idle 
  | Load Int Int Int 
  | Deliver Int Int Int

type alias Model =
  { schedule: List Packet
  , position: Point
  , maxLoad: Int
  , status: Status
  }

init : Int -> Point -> Model
init load point =
  { schedule = []
  , position = point
  , maxLoad = load
  , status = Idle
  }


load : Packet -> Model -> Model
load packet model =
  { model | schedule = List.append model.schedule [packet] }
  

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

update : Model -> Model
update model =
--  case model.status of 
--    _ ->
      let
        newSchedule = 
          if model.position == newPos then
            List.tail model.schedule
            |> Maybe.withDefault []
          else
            model.schedule 
     
        newPos = 
          List.head model.schedule 
          |> Maybe.map .address
          |> Maybe.withDefault model.position
          |> stepTo model.position 
        
      in
        { model 
        | position = newPos
        , schedule = newSchedule 
        }


view : Model -> Form
view {position} = 
  oval 5 5
    |> filled Color.white
    |> move (position.x, position.y)

isEmpty : Model -> Bool
isEmpty model =
  List.isEmpty model.schedule

weight : Model -> Int
weight model =
  model.schedule
  |> List.map .weight
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
