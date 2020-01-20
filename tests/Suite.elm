module Suite exposing (suite)

import DroneTests exposing (suite)
import PacketTests exposing (suite)
import PointTests exposing (suite)
import Test exposing (Test, describe)


suite : Test
suite =
    describe "Elm Drones Library Tests"
        [ PointTests.suite
        , DroneTests.suite
        , PacketTests.suite
        ]
