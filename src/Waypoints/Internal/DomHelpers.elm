module Waypoints.Internal.DomHelpers exposing (getBoundingClientRect, getWindowHeight, waitForRender, scrollPosition)

import Native.DomHelpers
import Json.Decode as JD exposing (Decoder)
import Waypoints.Internal.Types exposing (Position, BoundingClientRect)


getBoundingClientRect : String -> Result String BoundingClientRect
getBoundingClientRect elementId =
    Native.DomHelpers.getBoundingClientRect elementId
        |> JD.decodeValue boundingClientRectDecoder


boundingClientRectDecoder : Decoder BoundingClientRect
boundingClientRectDecoder =
    JD.map6 BoundingClientRect
        (JD.field "top" JD.float)
        (JD.field "right" JD.float)
        (JD.field "bottom" JD.float)
        (JD.field "left" JD.float)
        (JD.field "width" JD.float)
        (JD.field "height" JD.float)


getWindowHeight : () -> Float
getWindowHeight =
    Native.DomHelpers.getWindowHeight


waitForRender : () -> Platform.Task x ()
waitForRender () =
    Native.DomHelpers.waitForRender ()


scrollPosition : () -> Position
scrollPosition () =
    Native.DomHelpers.scrollPosition ()
