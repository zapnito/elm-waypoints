module DomHelpers exposing (getBoundingClientRect, getWindowHeight, BoundingClientRect, waitForRender)

import Native.DomHelpers
import Json.Decode as JD exposing (Decoder)


type alias BoundingClientRect =
    { top : Float
    , right : Float
    , bottom : Float
    , left : Float
    , width : Float
    , height : Float
    }


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
