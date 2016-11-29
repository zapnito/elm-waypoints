effect module Waypoints where { subscription = MySub } exposing (..)

import Task exposing (Task)
import Dict exposing (Dict)
import Dom.LowLevel as Dom
import Json.Decode as JD exposing (Decoder)
import Process
import DomHelpers


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
    , isScrollListenerEnabled : Bool
    }


type ElementVisibility
    = Hidden
    | Visible
    | NotFound


type Msg
    = Update


init : Task Never (State msg)
init =
    Task.succeed
        { enteredViewSubs = Dict.empty
        , elementStatuses = Dict.empty
        , isScrollListenerEnabled = False
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
            { state | enteredViewSubs = buildSubscriptions subs }
    in
        ensureScrollListenerIsEnabled
            |> Task.andThen (always <| DomHelpers.waitForRender ())
            |> Task.andThen (always <| Platform.sendToSelf router Update)
            |> Task.map (always updatedState)


startScrollListener router =
    Process.spawn
        (Dom.onDocument
            "scroll"
            (JD.succeed True)
            (always <| Platform.sendToSelf router Update)
        )


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
    case selfMsg of
        Update ->
            let
                ( updatedElementStatuses, callbackEffects ) =
                    updateElementStatuses
                        router
                        state.enteredViewSubs
                        state.elementStatuses
            in
                callbackEffects
                    |> Task.map (always <| { state | elementStatuses = updatedElementStatuses })


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
            List.foldl
                (\elementId statuses -> statuses |> Dict.insert elementId (visibilityOf elementId))
                Dict.empty
                (Dict.keys enteredViewSubs)

        chainCallbackEffects elementId updatedElementStatus taskChain =
            case ( elementStatuses |> Dict.get elementId, updatedElementStatus ) of
                ( Just Visible, Visible ) ->
                    taskChain

                ( _, Visible ) ->
                    taskChain
                        |> Task.andThen (always <| sendAllToApp router (taggersFor elementId))

                _ ->
                    taskChain
    in
        ( updatedElementStatuses
        , Dict.foldl chainCallbackEffects (Task.succeed ()) updatedElementStatuses
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
                NotFound


sendAllToApp router taggers =
    taggers
        |> List.map (Platform.sendToApp router)
        |> Task.sequence
        |> Task.map (always ())
