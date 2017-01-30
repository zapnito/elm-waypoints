module InView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Waypoints


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init =
    ( { inViewIds = [] }, Cmd.none )


type alias Model =
    { inViewIds : List String }


type Msg
    = InViewChanged (List String)


update msg model =
    Debug.log "model" <|
        case msg of
            InViewChanged inViewIds ->
                ( { model | inViewIds = inViewIds }, Cmd.none )


words =
    List.range 0 1000
        |> List.map toString
        |> List.map (\n -> "word" ++ n)


view model =
    let
        elementView n word =
            div [ id ("element:" ++ (toString n)) ] [ text word ]
    in
        div [ id "viewport" ] (words |> List.indexedMap elementView)


subscriptions model =
    let
        messageDomIds =
            words |> List.indexedMap (\n word -> "element:" ++ (toString n))
    in
        Waypoints.inViewChanged "viewport" messageDomIds InViewChanged
