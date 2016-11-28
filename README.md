# elm-waypoints
waypoints effect manager for Elm

## Install

As this is an effect manager and uses native code it isn't available at http://package.elm-lang.org. Instead you'll need to use https://github.com/gdotdesign/elm-github-install. Just add `"zapnito/elm-waypoints": "0.1.0 <= v < 1.0.0"`  to your elm-package.json as usual then run `$ elm-github-install`.

## Usage

```
type Msg =
    WaypointEnteredView String

update msg model =
    case msg of
        WaypointEnteredView waypointName ->
            ...

subscriptions : Model -> Sub Msg
subscriptions model =
    Waypoints.enteredView "element1" (WaypointEnteredView "waypoint1")


view : Model -> Html Msg
view model =
    div [ id "element1" ][ text "My waypoint" ]
```

See https://github.com/zapnito/elm-waypoints/tree/master/examples for a working example.
