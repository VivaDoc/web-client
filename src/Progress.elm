module Progress exposing (BarColor(..), progress)

{-| WARNING this module relies on the bulma css.
-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList, style)


type alias ProgressConfig =
    { height : String
    , width : String
    , bars : List BarConfig
    }


type alias BarConfig =
    { widthPercent : Float
    , color : BarColor
    , text : Maybe String
    }


type BarColor
    = Primary
    | Success
    | Danger
    | Info
    | Warning


progress : ProgressConfig -> Html msg
progress config =
    div
        [ style "display" "flex"
        , style "height" config.height
        , style "overflow" "hidden"
        , style "font-size" "1rem"
        , style "background-color" "#e9ecef"
        , style "border-radius" "0"
        , style "margin" "auto"
        , style "width" config.width
        ]
    <|
        List.map (renderBar config.height) config.bars


renderBar : String -> BarConfig -> Html msg
renderBar height config =
    div
        [ classList
            [ ( case config.color of
                    Primary ->
                        "has-background-primary"

                    Success ->
                        "has-background-success"

                    Danger ->
                        "has-background-danger"

                    Info ->
                        "has-background-info"

                    Warning ->
                        "has-background-warning"
              , True
              )
            ]
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "center"
        , style "text-align" "center"
        , style "white-space" "nowrap"
        , style "width" <| String.fromFloat config.widthPercent ++ "%"
        , style "height" height
        , style "color" "white"
        , style "overflow" "hidden"
        ]
        (case config.text of
            Nothing ->
                []

            Just someText ->
                [ text someText ]
        )
