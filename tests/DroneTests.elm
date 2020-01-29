module DroneTests exposing (suite)

--import List exposing ((::))
--import Maybe exposing (..)

import Drone exposing (Drone, Msg(..), init)
import Expect exposing (equal)
import Packet exposing (Packet)
import Point
import Test exposing (Test, describe, test)


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
                , test "init weightOf" <| \() -> Expect.equal 0 (Drone.weightOf sample)
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
