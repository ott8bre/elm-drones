module Packet exposing
    ( Packet
    , init
    , weightOf
    )

import Env exposing (ItemId)
import Point exposing (Point)


type alias Packet =
    { sender : Point
    , recipient : Point
    , item : ItemId
    , copies : Int
    }


init : ItemId -> Int -> Point -> Point -> Packet
init item copies from to =
    { sender = from
    , recipient = to
    , item = item
    , copies = copies
    }


weightOf : Packet -> Int
weightOf { item, copies } =
    copies * Env.weightOf item
