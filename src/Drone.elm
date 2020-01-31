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
        Take _ ->
            { drone | schedule = msg :: drone.schedule }

        Drop _ ->
            { drone | schedule = drone.schedule ++ [ msg ] }

        _ ->
            drone


update : Drone -> Drone
update model =
    let
        currentMsg =
            model.schedule |> List.head |> Maybe.withDefault NoOp
    in
    case currentMsg of
        Take p ->
            if p.sender == model.position then
                { model
                    | packets = model.packets ++ [ p ]
                    , schedule = List.drop 1 model.schedule
                }

            else
                { model | position = Point.stepTo model.position p.sender }

        Drop p ->
            if p.recipient == model.position then
                { model
                    | packets = List.filter (\x -> x /= p) model.packets
                    , schedule = List.drop 1 model.schedule
                }

            else
                { model | position = Point.stepTo model.position p.recipient }

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
                    Just <| Packet.weightOf p

                _ ->
                    Nothing

        takingWeight =
            drone.schedule
                |> List.filterMap f
                |> List.sum
    in
    takingWeight + weightOf drone + Packet.weightOf packet <= drone.maxLoad


weightOf : Drone -> Int
weightOf =
    .packets
        >> List.map Packet.weightOf
        >> List.sum
