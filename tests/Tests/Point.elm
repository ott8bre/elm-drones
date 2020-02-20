module Tests.Point exposing (suite)

--import Fuzz exposing (Fuzzer, float)

import Expect exposing (FloatingPointTolerance(..))
import Point exposing (..)
import Test exposing (Test)
import Util.Expect as Expect
import Util.Fuzz as Fuzz



{-
   xxx : () -> Expectation
   xxx f =
       f identity >> Expect.equal
-}


suite : Test
suite =
    Test.describe "All"
        --
        [ Test.fuzz Fuzz.point "Map identity on point has no effect" <|
            \point ->
                map identity point
                    |> Expect.equal point

        --
        , Test.fuzz2 Fuzz.point Fuzz.point "Distance is not negative" <|
            \pointA pointB ->
                pointA
                    |> distanceFrom pointB
                    |> Expect.atLeast 0

        --
        , Test.fuzz Fuzz.point "Distance of a point from itself is zero" <|
            \point ->
                point
                    |> distanceFrom point
                    |> Expect.exactly 0

        --
        , Test.fuzz2 Fuzz.point Fuzz.point "Distance is commutative" <|
            \pointA pointB ->
                Expect.exactly
                    (distanceFrom pointA pointB)
                    (distanceFrom pointB pointA)

        --, Test.test "-" <| \_ -> Point 0.00000001 0.0 |> distanceFrom origin |> Expect.atLeast 1
        ]



{-
   suite : Test
   suite =
       describe "Point"
           [ describe "mapX"
               [ fuzz Fuzz.point "has no effect on identity" <|
                   \p ->
                       p
                           |> mapX identity
                           |> Expect.equal p
               , fuzz point "preserve y" <|
                   \p ->
                       p
                           |> mapX negate
                           |> .y
                           |> Expect.within (Absolute 0.0) p.y
               ]
           , describe "mapY"
               [ fuzz point "has no effect on identity" <|
                   \p ->
                       mapY identity p
                           |> Expect.equal p
               ]
           , describe "map"
               [ fuzz point "has no effect on identity" <|
                   \p ->
                       map identity p
                           |> Expect.equal p
               ]
           , describe "dot"
               [ test "dot 3 (1,2) should be (3,6)" <|
                   \_ ->
                       dot 3 { x = 1, y = 2 }
                           |> Expect.equal { x = 3, y = 6 }
               , fuzz float "has no effect on origin" <|
                   \n ->
                       dot n origin
                           |> Expect.equal origin
               ]
           , describe "plus"
               [ test "plus (1,2) (2,4) should be (3,6)" <|
                   \_ ->
                       { x = 2, y = 4 } |> plus { x = 1, y = 2 } |> Expect.equal { x = 3, y = 6 }

               --
               , fuzz2 point point "has the same effect on swapped arguments" <|
                   \p q ->
                       plus p q
                           |> Expect.equal (plus q p)
               ]
           , describe "minus"
               [ test "minus (3,2) (1,1) should be (2,1)" <|
                   \_ ->
                       minus { x = 3, y = 2 } { x = 1, y = 1 }
                           |> Expect.equal { x = 2, y = 1 }
               , fuzz2 point point "is the opposite if swapped arguments" <|
                   \p q ->
                       minus p q
                           |> dot -1
                           |> Expect.equal (minus q p)
               ]
           , describe "distanceFrom"
               [ fuzz2 point point "is always not negative" <|
                   \p q ->
                       q |> distanceFrom p |> Expect.atLeast 0

               --
               , fuzz point "is zero if arguments are equal" <|
                   \p ->
                       p |> distanceFrom p |> Expect.within (Absolute 0.0) 0

               --
               , fuzz2 point point "is commutative" <|
                   \p q ->
                       let
                           qp =
                               q |> distanceFrom p

                           pq =
                               p |> distanceFrom q
                       in
                       qp |> Expect.within (Absolute 0.0) pq

               --
               , test "distance (1,1) (4,5) should be 5" <|
                   \_ ->
                       distanceFrom (Point 1 1) (Point 4 5)
                           |> Expect.within (Absolute 0.000000001) 5
               ]
           ]

-}
