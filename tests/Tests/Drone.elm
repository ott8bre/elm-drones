module Tests.Drone exposing (tests)

import Drone exposing (Drone, Msg(Take,Drop))
import Packet exposing (Packet) 
import Point 
-- exposing (Point)

import Basics exposing (..)
import List
import List exposing ((::))
import Maybe exposing (..)

import Test exposing (..)
import Expect exposing (..)

sample : Drone
sample = 
  Drone.init 1 Point.origin

packet : Packet
packet =
  Packet.init 1 1 Point.origin Point.origin
  
sampleTake : Drone
sampleTake = 
  Drone.enqueue (Take packet) sample 

tests : Test
tests =
  let 
    basicsTests = describe "Basics"
        [ test "init .schedule" <| \() -> Expect.equal [] (.schedule sample)
        , test "init .packets" <| \() -> Expect.equal [] (.packets sample)
        , test "init .position" <| \() -> Expect.equal Point.origin (.position sample)
        , test "init weight" <| \() -> Expect.equal 0 (Drone.weight sample)
        ]

    takeTests = describe "Take"
        [ test "one .schedule" <| \() -> Expect.equal 1 (sampleTake |> .schedule |> List.length)
        ]

  in
      describe "Drone"
        [ basicsTests
        , takeTests
        ]

