# elm-waypoints
#### Waypoints effect manager for Elm

Waypoints are elements on your page that you want to know about when they enter the browser's view. Some scenarios where this might be useful:

1) Tracking when a user has read a message, i.e. a messages is assumed "read" once the message is in view - see https://github.com/zapnito/elm-waypoints/tree/master/examples/MentionsTracker.elm

2) Infinite loading - Detect when user is scrolled to the bottom and load more results


## Install

As this is an effect manager and uses native code it isn't available at http://package.elm-lang.org. Instead you'll need to use https://github.com/gdotdesign/elm-github-install. Just add `"zapnito/elm-waypoints": "0.2.0 <= v < 1.0.0"`  to your elm-package.json as usual then run `$ elm-github-install`.


## Usage

```
type Msg =
    WaypointTriggered String

update msg model =
    case msg of
        WaypointTriggered message ->
            ...

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Waypoints.enteredView "element1" (WaypointTriggered "element entered window")
        ]


view : Model -> Html Msg
view model =
    div [ id "element1" ][ text "My waypoint" ]
```

See https://github.com/zapnito/elm-waypoints/tree/master/examples for a working example.
