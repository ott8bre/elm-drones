module Drone
    ( Drone
    , newDrone
    , take, drop, flyTo
    ) where

import Array exposing (..)

import Point exposing (..)

type Status = Idle | Load Int Int Int | Deliver Int Int Int

type alias Drone =
  { position: Point
  , products: Array Int
  , maxLoad: Int
  , status: Status
  }

newDrone : Int -> Int -> Float -> Float -> Drone
newDrone l n a b =
  { position=Point a b
  , products=repeat n 0
  , maxLoad=l
  , status=Idle
  }

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
