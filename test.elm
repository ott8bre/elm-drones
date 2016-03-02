import Graphics.Element exposing (..)
import Array exposing (..)

numberOfProducts = 5

type alias Point =
  { x: Float
  , y: Float
  }

type Status = Idle | Load Int Int Int | Deliver Int Int Int

type alias Drone =
  { position: Point
  , products: Array Int
  , status: Status
  }

drone : Int -> Float -> Float -> Drone
drone n a b =
  { position=Point a b
  , products=repeat n 0
  , status=Idle
  }

initDrone = drone numberOfProducts










main =
  show (initDrone 10 50)
