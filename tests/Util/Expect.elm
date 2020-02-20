module Util.Expect exposing (exactly)

import Expect exposing (Expectation)


exactly : Float -> Float -> Expectation
exactly actual expected =
    actual |> Expect.within (Expect.Absolute 0.0) expected
