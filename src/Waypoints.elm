effect module Waypoints where { subscription = MySub } exposing (..)

import Task exposing (Task)
import Dict exposing (Dict)
import Dom.LowLevel as Dom
import Json.Decode as JD exposing (Decoder)
import Process
import DomHelpers
import Time exposing (Time)


type MySub msg
    = EnteredView String msg


subMap : (a -> b) -> MySub a -> MySub b
subMap func sub =
    case sub of
        EnteredView elementId tagger ->
            EnteredView elementId (func tagger)


enteredView elementId tagger =
    subscription (EnteredView elementId tagger)


type alias State msg =
    { enteredViewSubs : Dict String (List msg)
    , elementStatuses : Dict String ElementVisibility
    , isActive : Bool
    , previousCheckAt : Maybe Time
    , lastThrottledAt : Time
    }


type ElementVisibility
    = Hidden
    | Visible
    | NotInDom


type Msg
    = Updated
    | Scrolled ScrollEvent
    | TrailingEdgeCheck Time


init : Task Never (State msg)
init =
    Task.succeed
        { enteredViewSubs = Dict.empty
        , elementStatuses = Dict.empty
        , isActive = False
        , previousCheckAt = Nothing
        , lastThrottledAt = 0
        }


onEffects :
    Platform.Router msg Msg
    -> List (MySub msg)
    -> State msg
    -> Task Never (State msg)
onEffects router subs state =
    if (List.isEmpty subs) && not state.isActive then
        Task.succeed state
    else
        Task.succeed state
            |> Task.andThen (activateIfIdle router subs)
            |> Task.andThen (handleOnEffects router subs)


activateIfIdle router subs state =
    if state.isActive then
        Task.succeed state
    else
        startScrollListener router
            |> Task.map (always { state | isActive = True })


handleOnEffects router subs state =
    let
        updatedState =
            { state | enteredViewSubs = buildSubscriptions subs }
    in
        DomHelpers.waitForRender ()
            |> Task.andThen (always <| Platform.sendToSelf router Updated)
            |> Task.map (always updatedState)


type alias ScrollEvent =
    { timeStamp : Time
    }


startScrollListener router =
    Process.spawn
        (Dom.onDocument
            "scroll"
            scrollEventDecoder
            (\scrollEvent -> Platform.sendToSelf router (Scrolled scrollEvent))
        )


scrollEventDecoder : Decoder ScrollEvent
scrollEventDecoder =
    JD.map ScrollEvent
        (JD.field "timeStamp" JD.float)


buildSubscriptions : List (MySub msg) -> Dict String (List msg)
buildSubscriptions subs =
    let
        flatten sub =
            case sub of
                EnteredView elementId tagger ->
                    ( elementId, tagger )

        addTagger tagger maybeList =
            Just <|
                case maybeList of
                    Just list ->
                        tagger :: list

                    Nothing ->
                        [ tagger ]

        groupByElementId ( elementId, tagger ) groups =
            groups |> Dict.update elementId (addTagger tagger)
    in
        subs
            |> List.map flatten
            |> List.foldl groupByElementId Dict.empty


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router selfMsg state =
    let
        applyUpdate func =
            let
                ( updatedElementStatuses, callbackEffects ) =
                    updateElementStatuses
                        router
                        state.enteredViewSubs
                        state.elementStatuses

                updatedState =
                    func { state | elementStatuses = updatedElementStatuses }
            in
                callbackEffects
                    |> Task.map (always updatedState)
    in
        case selfMsg of
            Updated ->
                applyUpdate identity

            Scrolled { timeStamp } ->
                if shouldThrottle state.previousCheckAt timeStamp then
                    scheduleSendToSelf router (TrailingEdgeCheck timeStamp)
                        |> Task.map (always { state | lastThrottledAt = timeStamp })
                else
                    applyUpdate (\state -> { state | previousCheckAt = Just timeStamp })

            TrailingEdgeCheck time ->
                if time == state.lastThrottledAt && (Just time) /= state.previousCheckAt then
                    applyUpdate (\state -> { state | previousCheckAt = Just time })
                else
                    (Task.succeed state)


scheduleSendToSelf router tagger =
    Process.sleep scrollThrottlePeriodMs
        |> Task.andThen (always <| Platform.sendToSelf router tagger)
        |> Process.spawn


shouldThrottle : Maybe Time -> Time -> Bool
shouldThrottle maybePrevious current =
    case maybePrevious of
        Nothing ->
            False

        Just previous ->
            current - previous < scrollThrottlePeriodMs


scrollThrottlePeriodMs =
    50


updateElementStatuses :
    Platform.Router msg Msg
    -> Dict String (List msg)
    -> Dict String ElementVisibility
    -> ( Dict String ElementVisibility, Task x () )
updateElementStatuses router enteredViewSubs elementStatuses =
    let
        addElementStatus elementId =
            Dict.insert elementId (visibilityOf elementId)

        taggersFor elementId =
            enteredViewSubs
                |> Dict.get elementId
                |> Maybe.withDefault []

        updatedElementStatuses =
            enteredViewSubs
                |> Dict.keys
                |> List.foldl
                    (\elementId statuses -> statuses |> Dict.insert elementId (visibilityOf elementId))
                    Dict.empty

        callbackEffects =
            updatedElementStatuses
                |> Dict.foldl
                    (\elementId updatedElementStatus taskChain ->
                        case ( elementStatuses |> Dict.get elementId, updatedElementStatus ) of
                            ( Just Visible, Visible ) ->
                                taskChain

                            ( _, Visible ) ->
                                taskChain
                                    |> Task.andThen (always <| elementCallbackEffects router (taggersFor elementId))

                            _ ->
                                taskChain
                    )
                    (Task.succeed ())
    in
        ( updatedElementStatuses
        , callbackEffects
        )


visibilityOf elementId =
    let
        windowHeight =
            DomHelpers.getWindowHeight ()
    in
        case DomHelpers.getBoundingClientRect elementId of
            Ok { top } ->
                if (0 < top) && (top < windowHeight) then
                    Visible
                else
                    Hidden

            Err message ->
                NotInDom


elementCallbackEffects router elementTaggers =
    elementTaggers
        |> List.foldl
            (\tagger taskChain ->
                taskChain |> Task.andThen (always <| Platform.sendToApp router tagger)
            )
            (Task.succeed ())
