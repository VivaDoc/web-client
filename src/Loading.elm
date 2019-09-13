module Loading exposing (renderLoadingScreen)

import Html exposing (div, text)
import Html.Attributes exposing (class, style)


renderLoadingScreen : String -> Html.Html msg
renderLoadingScreen loadingText =
    div
        [ class "section is-large" ]
        [ div
            [ class "has-text-centered title is-6 has-text-grey-light" ]
            [ text loadingText ]
        , div [ class "loader", style "margin" "10px auto" ] []
        ]
