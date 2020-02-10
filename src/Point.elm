module Point exposing
    ( Point
    , add
    , distance
    , dot
    , mapX
    , mapY
    , negate
    , origin
    , stepTo
    , sub
    )


type alias Point =
    { x : Float
    , y : Float
    }


origin : Point
origin =
    Point 0 0


negate : Point -> Point
negate { x, y } =
    { x = -x, y = -y }


distance : Point -> Point -> Float
distance p q =
    let
        { x, y } =
            sub p q
    in
    sqrt <| x * x + y * y


add : Point -> Point -> Point
add p q =
    { x = p.x + q.x
    , y = p.y + q.y
    }


sub : Point -> Point -> Point
sub p q =
    { x = p.x - q.x
    , y = p.y - q.y
    }


dot : Float -> Point -> Point
dot n p =
    { x = n * p.x
    , y = n * p.y
    }


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
            distance src dest
    in
    if d < 0.5 then
        dest

    else
        let
            p =
                1 / d
        in
        add (dot (1 - p) src) (dot p dest)
