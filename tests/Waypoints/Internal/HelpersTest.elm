module Waypoints.Internal.HelpersTest exposing (all)

import Test exposing (..)
import Expect
import Waypoints.Internal.Helpers
import Waypoints.Internal.Types exposing (Edge(..), Transition(..))


screenHeight =
    800


waypointRectWith =
    { top = 0
    , right = 1024
    , bottom = 800
    , left = 0
    , width = 1024
    , height = 800
    }


all =
    describe "Internal"
        [ expectHit Entered Top -50 50 screenHeight
        , expectNoHit Entered Top 50 -50 screenHeight
        , expectNoHit Entered Top -40 -50 screenHeight
        , expectNoHit Exited Top -50 50 screenHeight
        , expectHit Exited Top 50 -50 screenHeight
        , expectHit Crossed Top -50 50 screenHeight
        , expectHit Crossed Top 50 -50 screenHeight
        , expectNoHit Crossed Top 40 50 screenHeight
        , expectHit Exited Bottom (screenHeight - 100) (screenHeight + 100) screenHeight
        , expectNoHit Exited Bottom (screenHeight + 100) (screenHeight - 100) screenHeight
        , expectHit Entered Bottom (screenHeight + 100) (screenHeight - 100) screenHeight
        , expectNoHit Entered Bottom (screenHeight - 100) (screenHeight + 100) screenHeight
        ]


expectHit transition edge previousTop currentTop screenHeight =
    test ("when scroll " ++ (toString previousTop) ++ " => " ++ (toString currentTop)) <|
        \() ->
            Expect.true ("should fire: " ++ (toString transition) ++ "/" ++ (toString edge)) <|
                Waypoints.Internal.Helpers.isWaypointHit
                    edge
                    transition
                    previousTop
                    currentTop
                    screenHeight


expectNoHit transition edge previousTop currentTop screenHeight =
    test ("when scroll " ++ (toString previousTop) ++ " => " ++ (toString currentTop)) <|
        \() ->
            Expect.false ("should not fire: " ++ (toString transition) ++ "/" ++ (toString edge)) <|
                Waypoints.Internal.Helpers.isWaypointHit
                    edge
                    transition
                    previousTop
                    currentTop
                    screenHeight
