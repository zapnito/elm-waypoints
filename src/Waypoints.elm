effect module Waypoints
    where { subscription = MySub }
    exposing
        ( enteredTop
        , exitedTop
        , crossedTop
        , enteredBottom
        , exitedBottom
        , crossedBottom
        )

import Task exposing (Task)
import Dict exposing (Dict)
import Dom.LowLevel as Dom
import Json.Decode as JD exposing (Decoder)
import Process
import Waypoints.Internal.Helpers exposing (..)
import Waypoints.Internal.Types exposing (..)
import Waypoints.Internal.DomHelpers as DomHelpers


{-| This library lets you respond when elements scroll in/out of view
# Subscriptions
@docs enteredTop, exitedTop, crossedTop, enteredBottom, exitedBottom, crossedBottom
-}
type MySub msg
    = Waypoint Edge Transition String msg


subMap : (a -> b) -> MySub a -> MySub b
subMap func sub =
    case sub of
        Waypoint edge transition elementId tagger ->
            Waypoint edge transition elementId (func tagger)


{-| Subscribe to when a waypoint enters from the top of the window

     import Waypoints

     type Msg = WaypointHit | ...

     subscriptions model =
        Waypoints.enteredTop "elementId123" WaypointHit
-}
enteredTop : String -> msg -> Sub msg
enteredTop elementId tagger =
    subscription (Waypoint Top Entered elementId tagger)


{-| Subscribe to when a waypoint exits the top of the window

     import Waypoints

     type Msg = WaypointHit | ...

     subscriptions model =
        Waypoints.exitedTop "elementId123" WaypointHit
-}
exitedTop : String -> msg -> Sub msg
exitedTop elementId tagger =
    subscription (Waypoint Top Exited elementId tagger)


{-| Subscribe to when a waypoint crosses the top of the window

     import Waypoints

     type Msg = WaypointHit | ...

     subscriptions model =
        Waypoints.crossedTop "elementId123" WaypointHit
-}
crossedTop : String -> msg -> Sub msg
crossedTop elementId tagger =
    subscription (Waypoint Top Crossed elementId tagger)


{-| Subscribe to when a waypoint enters from the bottom of the window

     import Waypoints

     type Msg = WaypointHit | ...

     subscriptions model =
        Waypoints.enteredBottom "elementId123" WaypointHit
-}
enteredBottom : String -> msg -> Sub msg
enteredBottom elementId tagger =
    subscription (Waypoint Bottom Exited elementId tagger)


{-| Subscribe to when a waypoint exits the bottom of the window

     import Waypoints

     type Msg = WaypointHit | ...

     subscriptions model =
        Waypoints.exitedBottom "elementId123" WaypointHit
-}
exitedBottom : String -> msg -> Sub msg
exitedBottom elementId tagger =
    subscription (Waypoint Bottom Entered elementId tagger)


{-| Subscribe to when a waypoint crosses the bottom of the window

     import Waypoints

     type Msg = WaypointHit | ...

     subscriptions model =
        Waypoints.crossedBottom "elementId123" WaypointHit
-}
crossedBottom : String -> msg -> Sub msg
crossedBottom elementId tagger =
    subscription (Waypoint Bottom Crossed elementId tagger)


type alias State msg =
    { subs : List (MySub msg)
    , isScrollListenerEnabled : Bool
    , previousPosition : Position
    , previousScrollEventTimeStamp : Float
    }


type Msg
    = Update
    | Scroll ScrollEvent


init : Task Never (State msg)
init =
    Task.succeed
        { subs = []
        , isScrollListenerEnabled = False
        , previousPosition = DomHelpers.scrollPosition ()
        , previousScrollEventTimeStamp = 0
        }


onEffects :
    Platform.Router msg Msg
    -> List (MySub msg)
    -> State msg
    -> Task Never (State msg)
onEffects router subs state =
    let
        ensureScrollListenerIsEnabled =
            if state.isScrollListenerEnabled then
                Task.succeed state
            else
                startScrollListener router
                    |> Task.map (always { state | isScrollListenerEnabled = True })

        updatedState =
            { state | subs = subs }
    in
        ensureScrollListenerIsEnabled
            |> Task.andThen (always <| DomHelpers.waitForRender ())
            |> Task.andThen (always <| Platform.sendToSelf router Update)
            |> Task.map (always updatedState)


startScrollListener : Platform.Router msg Msg -> Task Never Process.Id
startScrollListener router =
    Process.spawn
        (Dom.onDocument
            "scroll"
            scrollEventDecoder
            (\scrollEvent -> Platform.sendToSelf router (Scroll scrollEvent))
        )


scrollEventDecoder : Decoder ScrollEvent
scrollEventDecoder =
    JD.map2 ScrollEvent
        (JD.at [ "target", "defaultView" ] scrollPositionDecoder)
        (JD.field "timeStamp" JD.float)


scrollPositionDecoder : Decoder Position
scrollPositionDecoder =
    JD.map2 Position
        (JD.field "scrollX" JD.float)
        (JD.field "scrollY" JD.float)


type alias ScrollEvent =
    { position : Position
    , timeStamp : Float
    }


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router selfMsg state =
    let
        callbackEffects =
            buildCallbackEffects state.previousPosition (DomHelpers.scrollPosition ()) state.subs
                |> sendAllToApp router
    in
        case selfMsg of
            Update ->
                callbackEffects
                    |> Task.map (always state)

            Scroll scrollEvent ->
                if shouldThrottle state.previousScrollEventTimeStamp scrollEvent.timeStamp then
                    Debug.log "throttling" (Task.succeed state)
                else
                    callbackEffects
                        |> Task.map (always <| { state | previousPosition = scrollEvent.position, previousScrollEventTimeStamp = scrollEvent.timeStamp })


shouldThrottle previous current =
    current - previous < scrollThrottlePeriodMs


scrollThrottlePeriodMs =
    50


buildCallbackEffects : Position -> Position -> List (MySub msg) -> List msg
buildCallbackEffects previousPosition currentPosition subs =
    let
        deltaY =
            currentPosition.y - previousPosition.y

        windowHeight =
            DomHelpers.getWindowHeight ()

        addCallbackEffect sub taggers =
            case sub of
                Waypoint edge transition elementId tagger ->
                    if isWaypointHit edge transition elementId deltaY windowHeight then
                        tagger :: taggers
                    else
                        taggers
    in
        subs |> List.foldl addCallbackEffect []


isWaypointHit : Edge -> Transition -> String -> Float -> Float -> Bool
isWaypointHit edge transition elementId deltaY windowHeight =
    case (DomHelpers.getBoundingClientRect elementId) of
        Err _ ->
            False

        Ok boundingClientRect ->
            let
                currentWaypointTop =
                    boundingClientRect.top

                previousWaypointTop =
                    currentWaypointTop - deltaY
            in
                Waypoints.Internal.Helpers.isWaypointHit
                    edge
                    transition
                    currentWaypointTop
                    previousWaypointTop
                    windowHeight


sendAllToApp : Platform.Router msg Msg -> List msg -> Task Never ()
sendAllToApp router taggers =
    taggers
        |> List.map (Platform.sendToApp router)
        |> Task.sequence
        |> Task.map (always ())
