module Env exposing 
  ( ItemId
  , weightOf
  , gameWidth , gameHeight
  , orders
  )

import Array exposing (..)

import Point exposing (..)


(gameWidth,gameHeight) = (600,400)
--(gameWidth,gameHeight) = (900,600)


type alias ItemId = 
  Int


weights : Array Int
weights = Array.fromList [3, 1, 7, 2, 5]


weightOf : ItemId -> Int
weightOf index = 
  Array.get index weights 
  |> Maybe.withDefault 0


type alias Order =
  { address: Point
  , items: List ItemId
  }

orders : List Order
orders =
  [ { address= Point 0200 0100, items= [0,2,0,0,1]}
  , { address= Point -150 0100, items= [0,0,0,3,0]}
  , { address= Point 0150 -150, items= [0,1,1,0,1]}
  , { address= Point -150 -150, items= [0,1,1,0,1]}
  ]