module Point exposing
    ( Point
    , distance
    , origin
    )


type alias Point =
    { x : Float
    , y : Float
    }


origin : Point
origin =
    Point 0 0


distance : Point -> Point -> Float
distance p q =
    let
        dx =
            p.x - q.x

        dy =
            p.y - q.y
    in
    sqrt <| dx * dx + dy * dy
