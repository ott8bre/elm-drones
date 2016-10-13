module Env exposing 
  ( .. )

import Array exposing (..)

import Point exposing (..)


(gameWidth,gameHeight) = (600,400)
--(gameWidth,gameHeight) = (900,600)

droneMaxLoad : Int
droneMaxLoad = 
  12

type alias ItemId = 
  Int


weights : Array Int
weights = Array.fromList [3, 1, 7, 2, 5]


weightOf : ItemId -> Int
weightOf index = 
  Array.get index weights 
  |> Maybe.withDefault 0


type alias Target =
  { address: Point
  , items: Array ItemId
  }

orders : List Target
orders =
  [ { address= Point 0200 0100, items= Array.fromList [0,2,0,0,1]}
  , { address= Point -150 0100, items= Array.fromList [0,0,0,3,0]}
  , { address= Point 0150 -150, items= Array.fromList [0,1,1,0,1]}
  , { address= Point -150 -150, items= Array.fromList [0,1,1,0,1]}
  ]

warehouses : List Target
warehouses =
  [ { address= Point 0000 0000, items= Array.fromList [1,2,3,4,5]}
  , { address= Point 0100 -100, items= Array.fromList [5,4,3,2,1]}
  ]

type alias Packet =
  { address : Point
  , content : 
    { item : ItemId 
    , copies : Int 
    }
  }

packetInit : ItemId -> Int -> Point -> Packet
packetInit item copies point =
  { address = point
  , content =
    { item = item
    , copies = copies
    }
  }

packetWeight : Packet -> Int
packetWeight {content} = 
  content.copies * weightOf content.item

orderToPackets : Target -> List Packet
orderToPackets order =
  let
    f = \x y -> packetInit x y order.address
    packs = Array.indexedMap f order.items
  in
    Array.toList packs 
    |> List.filter (\p -> p.content.copies /= 0)
