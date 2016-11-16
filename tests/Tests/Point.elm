module Tests.Point exposing (tests)

import Point exposing (Point)

import Basics exposing (..)

import Test exposing (..)
import Expect exposing (..)

tests : Test
tests =
  let basicsTests = describe "Basics"
        [ test "origin .x" <| \() -> Expect.equal 0 (.x Point.origin)
        , test "origin .y" <| \() -> Expect.equal 0 (.y Point.origin)
        , test "distance origin origin" <| \() -> Expect.equal 0 (Point.distance Point.origin Point.origin)
        , test "distance (1,1) (2,2)" <| \() -> Expect.equal (sqrt 2) (Point.distance (Point 1 1)( Point 2 2))
        ]

  in
      describe "Point"
        [ basicsTests
        ]

