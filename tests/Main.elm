port module Main exposing (..)

import Test exposing (..)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)

import Tests.Point as Point
import Tests.Packet as Packet
import Tests.Drone as Drone


tests : Test
tests =
    describe "Elm Drones Library Tests"
        [ Point.tests
        , Drone.tests
        , Packet.tests
        ]

main : TestProgram
main =
    run emit tests


port emit : ( String, Value ) -> Cmd msg
