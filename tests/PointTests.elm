module PointTests exposing (suite)

import Expect exposing (FloatingPointTolerance(..), equal, within)
import Point exposing (Point)
import Test exposing (Test, describe, test)


suite : Test
suite =
    let
        basicsTests =
            describe "Basics"
                [ test "origin .x" <|
                    \() -> Expect.equal 0 (.x Point.origin)
                , test "origin .y" <|
                    \() -> Expect.equal 0 (.y Point.origin)
                , test "add (1,2) (2,4)" <|
                    \() -> Expect.equal { x = 3, y = 6 } (Point.add { x = 1, y = 2 } { x = 2, y = 4 })
                , test "sub (3,2) (1,1)" <|
                    \() -> Expect.equal { x = 2, y = 1 } (Point.sub { x = 3, y = 2 } { x = 1, y = 1 })
                , test "dot 3 (1,2)" <|
                    \() -> Expect.equal { x = 3, y = 6 } (Point.dot 3 { x = 1, y = 2 })
                , test "distance origin origin" <|
                    \() -> Expect.within (Absolute 0.000000001) 0 (Point.distance Point.origin Point.origin)
                , test "distance (1,1) (2,2)" <|
                    \() -> Expect.within (Absolute 0.000000001) (sqrt 2) (Point.distance (Point 1 1) (Point 2 2))
                ]
    in
    describe "Point"
        [ basicsTests
        ]
