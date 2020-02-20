module Tests.Drone exposing (suite)

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
                [ test "init .schedule" <|
                    \() ->
                        .schedule sample |> Expect.equal []

                --
                , test "init .packets" <|
                    \() ->
                        .packets sample |> Expect.equal []

                --
                , test "init .position" <|
                    \() ->
                        .position sample |> Expect.equal Point.origin

                --
                , test "init weightOf" <|
                    \() ->
                        Drone.weightOf sample |> Expect.equal 0
                ]

        takeTests =
            describe "Take"
                [ test "one .schedule" <|
                    \() ->
                        sampleTake |> .schedule |> List.length |> Expect.equal 1
                ]
    in
    describe "Drone"
        [ basicsTests
        , takeTests
        ]
