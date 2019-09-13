module Bulma exposing (BulmaClass(..), BulmaColor(..), BulmaSize(..), sizeToString, withBulmaClasses)

import Html exposing (Attribute)
import Html.Attributes exposing (class)


{-| Common helpers for working with Bulma.
-}
type BulmaClass
    = BulmaSize BulmaSize
    | TextColor BulmaColor


withBulmaClasses : List BulmaClass -> String -> Attribute msg
withBulmaClasses bulmaClasses regularClasses =
    class <|
        regularClasses
            ++ " "
            ++ (List.map classToString bulmaClasses |> String.join " ")


classToString : BulmaClass -> String
classToString bulmaClass =
    case bulmaClass of
        BulmaSize bulmaSize ->
            sizeToString bulmaSize

        TextColor bulmaColor ->
            "has-text-" ++ colorToString bulmaColor


type BulmaColor
    = Primary
    | Success
    | Danger
    | Info
    | Warning
    | LightGrey
    | DarkGrey


colorToString : BulmaColor -> String
colorToString bulmaColor =
    case bulmaColor of
        Primary ->
            "primary"

        Success ->
            "success"

        Danger ->
            "danger"

        Info ->
            "info"

        Warning ->
            "warning"

        LightGrey ->
            "grey-light"

        DarkGrey ->
            "grey-dark"


type BulmaSize
    = BulmaSmall
    | BulmaMedium
    | BulmaLarge


sizeToString : BulmaSize -> String
sizeToString bulmaSize =
    case bulmaSize of
        BulmaSmall ->
            "is-small"

        BulmaMedium ->
            "is-medium"

        BulmaLarge ->
            "is-large"
