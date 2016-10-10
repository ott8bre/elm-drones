module Packet exposing (Packet, init, weight)

import Point exposing (..)
import Env exposing (..)

type alias Packet =
  { address : Point
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
  }

weight : Packet -> Int
weight {content} = 
  content.copies * Env.weightOf content.item