effect module Waypoints where { subscription = MySub } exposing (enteredView, exitedView, inViewChanged)

{-| This library provides subscriptions for when DOM elements come into view and leave view

@docs enteredView

@docs exitedView

@docs inViewChanged

-}

import Task exposing (Task)
import Dict exposing (Dict)
import Dom.LowLevel as Dom
import Json.Decode as JD exposing (Decoder)
import Process
import DomHelpers
import Time exposing (Time)
import DefaultDict exposing (DefaultDict)


type MySub msg
    = EnteredView String msg
    | ExitedView String msg
    | InViewChanged String (List String) (List String -> msg)


type alias InViewSubscription msg =
    { viewportId : String
    , itemIds : List String
    , tagger : List String -> msg
    }


type alias State msg =
    { subscriptions : Subscriptions msg
    , viewports : DefaultDict String Viewport
    , elementStatuses : Dict String ElementVisibility
    , isActive : Bool
    , previousCheckAt : Maybe Time
    , lastThrottledAt : Time
    , visibleElements : DefaultDict String (List String)
    }


type alias Subscriptions msg =
    { enteredView : DefaultDict String (List msg)
    , exitedView : DefaultDict String (List msg)
    , inViewChanged : DefaultDict String (List (InViewSubscription msg))
    }


type alias Viewport =
    Dict String ElementVisibility


type ElementVisibility
    = Offscreen
    | OnScreen
    | NotInDom


type Msg
    = Updated
    | Scrolled ScrollEvent
    | TrailingEdgeCheck Time


type alias ScrollEvent =
    { timeStamp : Time
    }


subMap : (a -> b) -> MySub a -> MySub b
subMap func sub =
    case sub of
        EnteredView elementId tagger ->
            EnteredView elementId (func tagger)

        ExitedView elementId tagger ->
            ExitedView elementId (func tagger)

        InViewChanged viewportId itemIds tagger ->
            InViewChanged viewportId itemIds (tagger >> func)


{-| Subscribe to DOM elements entering the view
-}
enteredView : String -> msg -> Sub msg
enteredView elementId tagger =
    subscription (EnteredView elementId tagger)


{-| Subscribe to DOM elements exiting the view
-}
exitedView : String -> msg -> Sub msg
exitedView elementId tagger =
    subscription (ExitedView elementId tagger)


{-| Subscribe to the list of DOM elements that are currently in view
-}
inViewChanged : String -> List String -> (List String -> msg) -> Sub msg
inViewChanged viewportId itemIds tagger =
    subscription (InViewChanged viewportId itemIds tagger)


init : Task Never (State msg)
init =
    Task.succeed
        { subscriptions = initSubscriptions
        , viewports = initViewports
        , elementStatuses = Dict.empty
        , isActive = False
        , previousCheckAt = Nothing
        , lastThrottledAt = 0
        , visibleElements = DefaultDict.empty []
        }


initViewports =
    DefaultDict.empty Dict.empty


initSubscriptions : Subscriptions msg
initSubscriptions =
    { enteredView = DefaultDict.empty []
    , exitedView = DefaultDict.empty []
    , inViewChanged = DefaultDict.empty []
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
            { state | subscriptions = buildSubscriptions subs }
    in
        DomHelpers.waitForRender ()
            |> Task.andThen (always <| Platform.sendToSelf router Updated)
            |> Task.map (always updatedState)


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


buildSubscriptions : List (MySub msg) -> Subscriptions msg
buildSubscriptions subs =
    let
        addSubscription : MySub msg -> Subscriptions msg -> Subscriptions msg
        addSubscription sub subscriptions =
            case sub of
                EnteredView elementId tagger ->
                    { subscriptions
                        | enteredView =
                            DefaultDict.update elementId
                                (\taggers -> Just (tagger :: taggers))
                                subscriptions.enteredView
                    }

                ExitedView elementId tagger ->
                    { subscriptions
                        | exitedView =
                            DefaultDict.update elementId
                                (\taggers -> Just (tagger :: taggers))
                                subscriptions.exitedView
                    }

                InViewChanged viewportId itemIds tagger ->
                    let
                        inViewSubscription : InViewSubscription msg
                        inViewSubscription =
                            { viewportId = viewportId
                            , itemIds = itemIds
                            , tagger = tagger
                            }
                    in
                        { subscriptions
                            | inViewChanged =
                                DefaultDict.update viewportId
                                    (\inViewSubscriptions -> Just (inViewSubscription :: inViewSubscriptions))
                                    subscriptions.inViewChanged
                        }
    in
        List.foldl addSubscription initSubscriptions subs


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router selfMsg state =
    case selfMsg of
        Updated ->
            update router state

        Scrolled { timeStamp } ->
            if shouldThrottle state.previousCheckAt timeStamp then
                scheduleSendToSelf router (TrailingEdgeCheck timeStamp)
                    |> Task.map (always { state | lastThrottledAt = timeStamp })
            else
                update router state |> setPreviousCheckAt timeStamp

        TrailingEdgeCheck time ->
            if time == state.lastThrottledAt && (Just time) /= state.previousCheckAt then
                update router state |> setPreviousCheckAt time
            else
                (Task.succeed state)


setPreviousCheckAt time chain =
    chain |> Task.map (\state -> { state | previousCheckAt = Just time })


update router state =
    Task.succeed state
        |> handleTransitionSubscriptions router
        |> handleInViewSubscriptions router


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


handleInViewSubscriptions :
    Platform.Router msg Msg
    -> Task Never (State msg)
    -> Task Never (State msg)
handleInViewSubscriptions router chain =
    let
        return =
            chain |> Task.andThen fireInViewChanged

        fireInViewChanged : State msg -> Task Never (State msg)
        fireInViewChanged state =
            state.subscriptions.inViewChanged
                |> DefaultDict.values
                |> List.concat
                |> List.foldl updateSubscription ( state, [] )
                |> mergeStateAndEffects

        mergeStateAndEffects : ( State msg, List (Task Never ()) ) -> Task Never (State msg)
        mergeStateAndEffects ( updatedState, effects ) =
            (Task.sequence effects) |> Task.map (always updatedState)

        updateSubscription : InViewSubscription msg -> ( State msg, List (Task Never ()) ) -> ( State msg, List (Task Never ()) )
        updateSubscription { viewportId, itemIds, tagger } ( state, effects ) =
            let
                subscriptionKey =
                    viewportId ++ (String.join "," itemIds)

                previous =
                    state.visibleElements |> DefaultDict.get subscriptionKey

                current =
                    itemIds |> List.filter (isVisible viewportId)
            in
                if previous /= current then
                    ( { state
                        | visibleElements =
                            DefaultDict.insert subscriptionKey current state.visibleElements
                      }
                    , Platform.sendToApp router (tagger current) :: effects
                    )
                else
                    ( state, effects )
    in
        return


handleTransitionSubscriptions :
    Platform.Router msg Msg
    -> Task Never (State msg)
    -> Task Never (State msg)
handleTransitionSubscriptions router chain =
    Task.andThen
        (\state ->
            let
                ( updatedStatuses, chain ) =
                    updateElementStatuses router state state.subscriptions state.elementStatuses
            in
                Task.map (always state) chain
        )
        chain


updateElementStatuses router state subscriptions elementStatuses =
    let
        subscriptions =
            state.subscriptions

        elementStatuses =
            state.elementStatuses

        addElementStatus elementId =
            Dict.insert elementId (visibilityOf elementId)

        allElementIds =
            (DefaultDict.keys subscriptions.enteredView) ++ (DefaultDict.keys subscriptions.exitedView)

        updatedElementStatuses =
            List.foldl
                (\elementId statuses -> statuses |> Dict.insert elementId (visibilityOf elementId))
                Dict.empty
                allElementIds

        enteredViewtaggersFor elementId =
            subscriptions.enteredView |> DefaultDict.get elementId

        exitedViewtaggersFor elementId =
            subscriptions.exitedView |> DefaultDict.get elementId

        fireTaggers taggers taskChain =
            taskChain
                |> Task.andThen (always <| elementCallbackEffects router taggers)

        updatedState =
            { state | elementStatuses = updatedElementStatuses }

        effects =
            Dict.foldl
                (\elementId updatedElementStatus taskChain ->
                    case ( elementStatuses |> Dict.get elementId, updatedElementStatus ) of
                        ( Just OnScreen, OnScreen ) ->
                            taskChain

                        ( _, OnScreen ) ->
                            taskChain |> fireTaggers (enteredViewtaggersFor elementId)

                        ( Just OnScreen, Offscreen ) ->
                            taskChain |> fireTaggers (exitedViewtaggersFor elementId)

                        _ ->
                            taskChain
                )
                (Task.succeed ())
                updatedElementStatuses
    in
        ( updatedElementStatuses, effects )


visibilityOf elementId =
    let
        windowHeight =
            DomHelpers.getWindowHeight ()
    in
        case DomHelpers.getBoundingClientRect elementId of
            Ok { top } ->
                if top |> isBetween 0 windowHeight then
                    OnScreen
                else
                    Offscreen

            Err message ->
                NotInDom


isBetween min max value =
    (min <= value) && (value <= max)


isVisible viewportId itemId =
    let
        itemRect =
            DomHelpers.getBoundingClientRect itemId

        viewportRect =
            DomHelpers.getBoundingClientRect viewportId
    in
        case Result.map2 isInBounds viewportRect itemRect of
            Ok True ->
                True

            _ ->
                False


isInBounds viewportRect itemRect =
    let
        windowHeight =
            DomHelpers.getWindowHeight ()

        topBound =
            max 0 viewportRect.top

        bottomBound =
            min windowHeight viewportRect.bottom
    in
        itemRect.top |> isBetween topBound bottomBound


elementCallbackEffects router elementTaggers =
    elementTaggers
        |> List.foldl
            (\tagger taskChain ->
                taskChain |> Task.andThen (always <| Platform.sendToApp router tagger)
            )
            (Task.succeed ())
