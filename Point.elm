module Point exposing
    ( Point
    , origin
    , distance
    ) 

type alias Point =
  { x: Float
  , y: Float
  }

origin = Point 0 0

distance : Point -> Point -> Float
distance p q =
  let
    square a = a*a
    sum =  ( square(p.x-q.x) + square(p.y-q.y) )
  in
    sum |> sqrt
