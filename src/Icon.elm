module Icon exposing (IconSize(..), RenderIconConfig, renderIcon)

{-| Helper for working with icons. This relies on Bulma for css.
-}

import Bulma
import Html exposing (Attribute, Html, div, i, span, text)
import Html.Attributes exposing (class, style)
import Words


type alias RenderIconConfig =
    { iconName : String
    , optionalAdjacentText : Maybe ( String, Bulma.BulmaColor )
    , iconSize : Bulma.BulmaSize
    , iconColor : Bulma.BulmaColor
    }


type IconSize
    = SmallIcon
    | MediumIcon
    | LargeIcon


renderIcon : RenderIconConfig -> Html msg
renderIcon { iconName, optionalAdjacentText, iconSize, iconColor } =
    let
        icon =
            span
                [ Bulma.withBulmaClasses [ Bulma.BulmaSize iconSize ] "icon" ]
                [ i
                    [ Bulma.withBulmaClasses
                        [ Bulma.TextColor iconColor ]
                        "material-icons"
                    , style "font-size" <| Words.pixelify <| iconSizeToPixels iconSize
                    ]
                    [ text iconName ]
                ]
    in
    case optionalAdjacentText of
        Nothing ->
            icon

        Just ( adjacentText, textColor ) ->
            div
                [ class "level is-mobile", style "margin" "0" ]
                [ div
                    [ class "level-left" ]
                    [ icon
                    , span
                        [ Bulma.withBulmaClasses
                            [ Bulma.TextColor textColor ]
                            ""
                        , style "margin-left" "10px"
                        ]
                        [ text adjacentText ]
                    ]
                ]


iconSizeToPixels : Bulma.BulmaSize -> Int
iconSizeToPixels bulmaSize =
    case bulmaSize of
        Bulma.BulmaSmall ->
            24

        Bulma.BulmaMedium ->
            36

        Bulma.BulmaLarge ->
            48
