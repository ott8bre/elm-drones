module Suite exposing (..)

--import Expect exposing (Expectation)
--import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import PointTests exposing (..)
import DroneTests exposing (..)
import PacketTests exposing (..)

suite : Test
suite =
    describe "Elm Drones Library Tests"
        [ PointTests.suite
        , DroneTests.suite
        , PacketTests.suite
        ]