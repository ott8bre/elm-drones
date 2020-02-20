module Point exposing
    ( Point
    , distanceFrom
    , dot
    , map
    , mapX
    , mapY
    , minus
    , origin
    , plus
    , stepTo
    )


type alias Point =
    { x : Float
    , y : Float
    }


origin : Point
origin =
    Point 0 0


distanceFrom : Point -> Point -> Float
distanceFrom p q =
    let
        { x, y } =
            minus p q
    in
    sqrt <| x * x + y * y


plus : Point -> Point -> Point
plus p q =
    { x = p.x + q.x
    , y = p.y + q.y
    }


minus : Point -> Point -> Point
minus p q =
    { x = p.x - q.x
    , y = p.y - q.y
    }


dot : Float -> Point -> Point
dot n p =
    { x = n * p.x
    , y = n * p.y
    }


map : (Float -> Float) -> Point -> Point
map f { x, y } =
    { x = f x, y = f y }


mapX : (Float -> Float) -> Point -> Point
mapX f { x, y } =
    { x = f x, y = y }


mapY : (Float -> Float) -> Point -> Point
mapY f { x, y } =
    { x = f x, y = y }


stepTo : Point -> Point -> Point
stepTo src dest =
    let
        d =
            distanceFrom src dest
    in
    if d < 0.5 then
        dest

    else
        let
            p =
                1 / d
        in
        plus (dot (1 - p) src) (dot p dest)
