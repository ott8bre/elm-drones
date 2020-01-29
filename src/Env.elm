module Env exposing (..)

import Array exposing (Array)
import Point exposing (Point)


droneMaxLoad : Int
droneMaxLoad =
    10


numberOfDrones : Int
numberOfDrones =
    8


weights : Array Int
weights =
    Array.fromList [ 3, 1, 7, 2, 5 ]


weightOf : Int -> Int
weightOf index =
    Array.get index weights
        |> Maybe.withDefault 0


type alias Target =
    { position : Point
    , items : Array Int
    }


makeTarget : Float -> Float -> List Int -> Target
makeTarget x y quantities =
    { position = Point x y
    , items = Array.fromList quantities
    }


orders : List Target
orders =
    [ makeTarget 200 100 [ 0, 2, 0, 0, 1 ]
    , makeTarget -150 100 [ 0, 0, 0, 3, 0 ]
    , makeTarget 150 -150 [ 0, 1, 1, 0, 1 ]
    , makeTarget 250 50 [ 2, 0, 0, 0, 1 ]
    , makeTarget -150 -150 [ 0, 1, 1, 0, 1 ]
    , makeTarget 50 50 [ 2, 0, 0, 0, 1 ]
    , makeTarget -250 190 [ 2, 0, 0, 0, 1 ]
    , makeTarget -50 -50 [ 2, 0, 0, 0, 1 ]
    , makeTarget -250 -190 [ 2, 0, 0, 0, 1 ]
    ]


warehouses : List Target
warehouses =
    [ makeTarget -100 0 [ 1, 2, 3, 4, 5 ]
    , makeTarget 100 0 [ 5, 0, 3, 0, 1 ]
    ]
