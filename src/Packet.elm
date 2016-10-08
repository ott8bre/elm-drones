module Packet exposing (..)

import Point exposing (..)
import Env exposing (..)

type alias Packet =
  { address : Point
  , weight : Int
  , content : 
    { item : ItemId 
    , copies : Int 
    }
  }

init : ItemId -> Int -> Point -> Packet
init item copies point =
  { address = point
  , content =
    { item = item
    , copies = copies
    }
  , weight = copies * Env.weightOf item
  }