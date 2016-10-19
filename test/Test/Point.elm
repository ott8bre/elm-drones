module Test.Point exposing (tests)

import Point exposing (Point)

import Basics exposing (..)

import ElmTest exposing (..)

tests : Test
tests =
  let basicsTests = suite "Basics"
        [ test "origin .x" <| assertEqual 0 (.x Point.origin)
        , test "origin .y" <| assertEqual 0 (.y Point.origin)
        , test "distance origin origin" <| assertEqual 0 (Point.distance Point.origin Point.origin)
        , test "distance (1,1) (2,2)" <| assertEqual (sqrt 2) (Point.distance (Point 1 1)( Point 2 2))
        ]

  in
      suite "Point"
        [ basicsTests
        ]

