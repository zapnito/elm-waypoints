module Tests exposing (..)

import Test exposing (..)
import Expect
import Waypoints.Internal.HelpersTest


all : Test
all =
    describe "Waypoints"
        [ Waypoints.Internal.HelpersTest.all
        ]
