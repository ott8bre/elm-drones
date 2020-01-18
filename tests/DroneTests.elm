module DroneTests exposing (suite)

-- exposing (Point)

import Basics exposing (..)
import Drone exposing (..)
import Expect exposing (..)
import List exposing ((::))
import Maybe exposing (..)
import Packet exposing (Packet)
import Point
import Test exposing (..)


sample : Drone
sample =
    Drone.init 1 Point.origin


packet : Packet
packet =
    Packet.init 1 1 Point.origin Point.origin


sampleTake : Drone
sampleTake =
    Drone.enqueue (Take packet) sample


suite : Test
suite =
    let
        basicsTests =
            describe "Basics"
                [ test "init .schedule" <| \() -> Expect.equal [] (.schedule sample)
                , test "init .packets" <| \() -> Expect.equal [] (.packets sample)
                , test "init .position" <| \() -> Expect.equal Point.origin (.position sample)
                , test "init weight" <| \() -> Expect.equal 0 (Drone.weight sample)
                ]

        takeTests =
            describe "Take"
                [ test "one .schedule" <| \() -> Expect.equal 1 (sampleTake |> .schedule |> List.length)
                ]
    in
    describe "Drone"
        [ basicsTests
        , takeTests
        ]
