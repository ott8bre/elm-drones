module Drone exposing
    ( Drone
    , Msg(..)
    , canLoad
    , enqueue
    , init
    , isEmpty
    , update
    , weightOf
    )

import Packet exposing (Packet)
import Point exposing (Point)


type Msg
    = Take Packet
    | Drop Packet
    | NoOp


type alias Drone =
    { schedule : List Msg
    , packets : List Packet
    , position : Point
    , maxLoad : Int
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
    case msg of
        Take p ->
            { drone | schedule = msg :: drone.schedule }

        Drop p ->
            { drone | schedule = drone.schedule ++ [ msg ] }

        _ ->
            drone


stepTo : Point -> Point -> Point
stepTo src dest =
    let
        d =
            Point.distance src dest |> floor

        p =
            if d == 0 then
                1

            else
                1 / toFloat d
    in
    { src
        | x = (1 - p) * src.x + p * dest.x
        , y = (1 - p) * src.y + p * dest.y
    }


update : Drone -> Drone
update model =
    let
        currentMsg =
            model.schedule |> List.head |> Maybe.withDefault NoOp
    in
    case currentMsg of
        Take p ->
            if p.sender == model.position then
                { model | packets = model.packets ++ [ p ], schedule = List.drop 1 model.schedule }

            else
                { model | position = stepTo model.position p.sender }

        Drop p ->
            if p.recipient == model.position then
                { model | packets = List.filter (\x -> x /= p) model.packets, schedule = List.drop 1 model.schedule }

            else
                { model | position = stepTo model.position p.recipient }

        _ ->
            model


isEmpty : Drone -> Bool
isEmpty model =
    List.isEmpty model.packets && List.isEmpty model.schedule


canLoad : Packet -> Drone -> Bool
canLoad packet drone =
    let
        f msg =
            case msg of
                Take p ->
                    Packet.weightOf p

                _ ->
                    0

        takingWeight =
            drone.schedule
                |> List.map f
                |> List.sum
    in
    takingWeight + weightOf drone + Packet.weightOf packet <= drone.maxLoad


weightOf : Drone -> Int
weightOf model =
    model.packets
        |> List.map Packet.weightOf
        |> List.foldr (+) 0
