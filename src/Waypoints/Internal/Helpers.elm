module Waypoints.Internal.Helpers exposing (..)

import Waypoints.Internal.Types exposing (..)


isWaypointHit : Edge -> Transition -> Float -> Float -> Float -> Bool
isWaypointHit edge transition previousTop currentTop screenHeight =
    let
        topOfScreen =
            0

        bottomOfScreen =
            screenHeight

        conditions =
            case ( transition, edge ) of
                ( Entered, Top ) ->
                    [ crossedDown topOfScreen previousTop currentTop ]

                ( Exited, Top ) ->
                    [ crossedUp topOfScreen previousTop currentTop ]

                ( Crossed, Top ) ->
                    [ crossedDown topOfScreen previousTop currentTop
                    , crossedUp topOfScreen previousTop currentTop
                    ]

                ( Entered, Bottom ) ->
                    [ crossedUp bottomOfScreen previousTop currentTop ]

                ( Exited, Bottom ) ->
                    [ crossedDown bottomOfScreen previousTop currentTop ]

                ( Crossed, Bottom ) ->
                    [ crossedUp bottomOfScreen previousTop currentTop
                    , crossedDown bottomOfScreen previousTop currentTop
                    ]
    in
        List.any identity conditions


crossedDown : Float -> Float -> Float -> Bool
crossedDown referencePoint previousTop currentTop =
    previousTop < referencePoint && referencePoint < currentTop


crossedUp : Float -> Float -> Float -> Bool
crossedUp referencePoint previousTop currentTop =
    previousTop > referencePoint && referencePoint > currentTop
