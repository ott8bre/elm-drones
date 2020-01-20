module Env exposing (..)

import Array exposing (..)
import Point exposing (..)


droneMaxLoad : Int
droneMaxLoad =
    10


numberOfDrones : Int
numberOfDrones =
    8


type alias ItemId =
    Int


weights : Array Int
weights =
    Array.fromList [ 3, 1, 7, 2, 5 ]


weightOf : ItemId -> Int
weightOf index =
    Array.get index weights
        |> Maybe.withDefault 0


type alias Target =
    { position : Point
    , items : Array ItemId
    }


orders : List Target
orders =
    [ { position = Point 200 100, items = Array.fromList [ 0, 2, 0, 0, 1 ] }
    , { position = Point -150 100, items = Array.fromList [ 0, 0, 0, 3, 0 ] }
    , { position = Point 150 -150, items = Array.fromList [ 0, 1, 1, 0, 1 ] }
    , { position = Point 250 50, items = Array.fromList [ 2, 0, 0, 0, 1 ] }
    , { position = Point -150 -150, items = Array.fromList [ 0, 1, 1, 0, 1 ] }
    , { position = Point 50 50, items = Array.fromList [ 2, 0, 0, 0, 1 ] }
    , { position = Point -250 190, items = Array.fromList [ 2, 0, 0, 0, 1 ] }
    , { position = Point -50 -50, items = Array.fromList [ 2, 0, 0, 0, 1 ] }
    , { position = Point -250 -190, items = Array.fromList [ 2, 0, 0, 0, 1 ] }
    ]


warehouses : List Target
warehouses =
    [ { position = Point -100 0, items = Array.fromList [ 1, 2, 3, 4, 5 ] }
    , { position = Point 100 0, items = Array.fromList [ 5, 0, 3, 0, 1 ] }
    ]
