module PointTests exposing (suite)

import Expect exposing (FloatingPointTolerance(..), equal, within)
import Fuzz exposing (..)
import Point exposing (Point)
import Random
import Shrink
import Test exposing (..)


point : Fuzzer Point
point =
    Fuzz.custom
        (Random.map2 Point (Random.float -100 100) (Random.float -100 100))
        (\{ x, y } -> Shrink.map Point (Shrink.float x) |> Shrink.andMap (Shrink.float y))


suite : Test
suite =
    describe "Point"
        [ describe "mapX"
            [ fuzz point "has no effect on identity" <|
                \p ->
                    Point.mapX identity p
                        |> Expect.equal p
            ]
        , describe "mapY"
            [ fuzz point "has no effect on identity" <|
                \p ->
                    Point.mapY identity p
                        |> Expect.equal p
            ]

        --, describe "bimap"
        --    []
        , describe "dot"
            [ test "dot 3 (1,2) should be (3,6)" <|
                \_ ->
                    Point.dot 3 { x = 1, y = 2 }
                        |> Expect.equal { x = 3, y = 6 }
            , fuzz float "has no effect on origin" <|
                \n ->
                    Point.dot n Point.origin
                        |> Expect.equal Point.origin
            ]
        , describe "add"
            [ test "add (1,2) (2,4) should be (3,6)" <|
                \_ ->
                    Point.add { x = 1, y = 2 } { x = 2, y = 4 }
                        |> Expect.equal { x = 3, y = 6 }
            , fuzz2 point point "has the same effect on swapped arguments" <|
                \p q ->
                    Point.add p q
                        |> Expect.equal (Point.add q p)
            ]
        , describe "sub"
            [ test "sub (3,2) (1,1) should be (2,1)" <|
                \_ ->
                    Point.sub { x = 3, y = 2 } { x = 1, y = 1 }
                        |> Expect.equal { x = 2, y = 1 }
            , fuzz2 point point "is the opposite if swapped arguments" <|
                \p q ->
                    Point.sub p q
                        |> Point.negate
                        |> Expect.equal (Point.sub q p)
            ]
        , describe "distance"
            [ fuzz2 point point "is always not negative" <|
                \p q ->
                    Point.distance p q
                        |> Expect.atLeast 0
            , fuzz point "is zero if arguments are equal" <|
                \p ->
                    Point.distance p p
                        |> Expect.within (Absolute 0.000000001) 0
            , test "distance (1,1) (4,5) should be 5" <|
                \_ ->
                    Point.distance (Point 1 1) (Point 4 5)
                        |> Expect.within (Absolute 0.000000001) 5
            ]
        ]
