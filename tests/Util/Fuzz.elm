module Util.Fuzz exposing (point)

import Fuzz exposing (Fuzzer)
import Point exposing (Point)
import Random
import Shrink


point : Fuzzer Point
point =
    Fuzz.custom (Random.map2 Point (Random.float -10 10) (Random.float -10 10)) <|
        \{ x, y } ->
            Shrink.map Point (Shrink.float x)
                |> Shrink.andMap (Shrink.float y)
