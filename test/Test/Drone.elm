module Test.Drone exposing (tests)

import Drone exposing (Drone, Msg(Take,Drop))
import Packet exposing (Packet) 
import Point 
-- exposing (Point)

import Basics exposing (..)
import List
import List exposing ((::))
import Maybe exposing (..)

import ElmTest exposing (..)

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
    basicsTests = suite "Basics"
        [ test "init .schedule" <| assertEqual [] (.schedule sample)
        , test "init .packets" <| assertEqual [] (.packets sample)
        , test "init .position" <| assertEqual Point.origin (.position sample)
        , test "init weight" <| assertEqual 0 (Drone.weight sample)
        ]

    takeTests = suite "Take"
        [ test "one .schedule" <| assertEqual 1 (sampleTake |> .schedule |> List.length)
        ]

  in
      suite "Drone"
        [ basicsTests
        , takeTests
        ]

