module Waypoints.Internal.Types exposing (..)


type alias BoundingClientRect =
    { top : Float
    , right : Float
    , bottom : Float
    , left : Float
    , width : Float
    , height : Float
    }


type alias Position =
    { x : Float
    , y : Float
    }


type Edge
    = Top
    | Bottom


type Transition
    = Entered
    | Exited
    | Crossed
