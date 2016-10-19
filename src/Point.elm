module Point exposing
    ( Point
    , origin
    , distance
    ) 

type alias Point =
  { x: Float
  , y: Float
  }

origin : Point 
origin = 
  Point 0 0

distance : Point -> Point -> Float
distance p q =
  let
    square a = a*a
  in
    square(p.x-q.x) + square(p.y-q.y) |> sqrt
