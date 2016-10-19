module Main exposing (..)

import Basics exposing (..)
import ElmTest exposing (..)

import Test.Point as Point
import Test.Packet as Packet
import Test.Drone as Drone

tests : Test
tests =
    suite "Elm Drones Library Tests"
        [ Point.tests
        , Drone.tests
        , Packet.tests
        ]

main : Program Never
main =
    runSuite tests
