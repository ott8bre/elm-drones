module Test.Packet exposing (tests)

import Env
import Packet exposing (Packet)
import Point

import Basics exposing (..)

import ElmTest exposing (..)

sample : Packet
sample = 
  Packet.init 1 1 Point.origin Point.origin


tests : Test
tests =
  let basicsTests = suite "Basics"
        [ test "init .item" <| assertEqual 1 (.item sample)
        , test "init .copies" <| assertEqual 1 (.copies sample)
        , test "init .sender" <| assertEqual Point.origin (.sender sample)
        , test "init .recipient" <| assertEqual Point.origin (.recipient sample)
        , test "init weight" <| assertEqual (Env.weightOf 1) (Packet.weight sample)
        ]

  in
      suite "Packet"
        [ basicsTests
        ]

