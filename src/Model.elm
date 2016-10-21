module Model exposing (Game, State(Play, Pause, End))

import Window exposing (Size)

import Packet exposing (Packet)
import Drone exposing (Drone, Msg(Take,Drop))

type State 
  = Play 
  | Pause
  | End

type alias Game =
  { state : State
  , turn : Int
  , size : Size
  , drones : List Drone
  , packets : List Packet
  , products : List Int
  }