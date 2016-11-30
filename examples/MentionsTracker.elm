module WaypointsExample exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Waypoints
import Set exposing (Set)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { messages : List Message
    , mentions : Set Int
    }


type alias Message =
    { id : Int
    , body : String
    }


messageCount =
    1000


mentions =
    Set.fromList [ 100, 500, 1000 ]


username =
    "Hubert Blaine Wolfeschlegelsteinhausenbergerdorff"


initMessages =
    let
        messageBody n =
            ("message" ++ (toString n))
                ++ (if Set.member n mentions then
                        " @" ++ username
                    else
                        ""
                   )
    in
        List.map
            (\n -> Message n (messageBody n))
            (List.range 1 messageCount)


init =
    ( { messages = initMessages
      , mentions = mentions
      }
    , Cmd.none
    )


type Msg
    = MessageRead Int


update msg model =
    case msg of
        MessageRead messageId ->
            ( { model | mentions = Set.remove messageId model.mentions }
            , Cmd.none
            )


view model =
    div []
        [ notificationView model.mentions
        , messagesView model.messages
        ]


notificationView mentions =
    let
        styles =
            [ ( "position", "fixed" )
            , ( "top", "40%" )
            , ( "right", "50%" )
            , ( "font-size", "4rem" )
            ]
    in
        div [ style styles ] [ text ("unread " ++ (toString <| Set.size mentions)) ]


messagesView messages =
    ul [] <| (messages |> List.map messageView)


messageView message =
    li [ id (messageElementId message.id) ] [ text message.body ]


messageElementId messageId =
    "message:" ++ (toString messageId)


subscriptions model =
    let
        messageWaypoint mention =
            Sub.batch
                [ Waypoints.crossedTop (messageElementId mention) (MessageRead mention)
                , Waypoints.crossedBottom (messageElementId mention) (MessageRead mention)
                ]
    in
        model.mentions
            |> Set.toList
            |> List.map messageWaypoint
            |> Sub.batch
