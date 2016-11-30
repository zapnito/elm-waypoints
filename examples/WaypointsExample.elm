module WaypointsExample exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set
import Waypoints


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { currentElement : Int
    , showWords : Bool
    }


type Msg
    = Appeared String
    | ChangeElement
    | ToggleWords


init : ( Model, Cmd Msg )
init =
    ( { currentElement = 1, showWords = False }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Appeared elementId ->
            Debug.log ("appeared: " ++ elementId) ( model, Cmd.none )

        ToggleWords ->
            ( { model | showWords = not model.showWords }, Cmd.none )

        ChangeElement ->
            ( { model | currentElement = model.currentElement + 1 }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        elementStyle n =
            if n == model.currentElement then
                [ ( "font-weight", "bold" ) ]
            else
                []

        elementView n =
            div [ id ("elem" ++ (toString n)), style (elementStyle n) ]
                [ text ("word" ++ (toString n)) ]

        toggleLabel =
            if model.showWords then
                "Hide words"
            else
                "Show words"
    in
        div []
            [ h1 [] [ text "Demo" ]
            , ul []
                [ li [] [ text "Hit 'Show words'" ]
                , li [] [ text "Open developer console" ]
                , li [] [ text "Scroll highlighted word off screen and back on again, you should see the subscription fire in the console" ]
                ]
            , button [ onClick ToggleWords ] [ text toggleLabel ]
            , button [ onClick ChangeElement ] [ text "Attach waypoint to next word" ]
            , if model.showWords then
                div [] <| (List.range 1 100 |> List.map elementView)
              else
                text ""
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        elementId =
            "elem" ++ (toString model.currentElement)
    in
        Waypoints.crossedTop elementId (Appeared elementId)
