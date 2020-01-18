module PacketTests exposing (suite)

import Env
import Packet exposing (Packet)
import Point

import Basics exposing (..)

import Test exposing (..)
import Expect exposing (..)

sample : Packet
sample = 
  Packet.init 1 1 Point.origin Point.origin


suite : Test
suite =
  let basicsTests = describe "Basics"
        [ test "init .item" <| \() -> Expect.equal 1 (.item sample)
        , test "init .copies" <| \() -> Expect.equal 1 (.copies sample)
        , test "init .sender" <| \() -> Expect.equal Point.origin (.sender sample)
        , test "init .recipient" <| \() -> Expect.equal Point.origin (.recipient sample)
        , test "init weight" <| \() -> Expect.equal (Env.weightOf 1) (Packet.weight sample)
        ]

  in
      describe "Packet"
        [ basicsTests
        ]

