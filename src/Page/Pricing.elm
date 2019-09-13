module Page.Pricing exposing (Model, Msg(..), init, update, view)

import Html exposing (div, h1, p, text)
import Html.Attributes exposing (class, style)
import Session


type alias Model =
    { session : Session.Session }


type Msg
    = NoOp


init : Session.Session -> ( Model, Cmd.Cmd Msg )
init session =
    ( { session = session }, Cmd.none )


view : Model -> { title : String, content : Html.Html Msg }
view model =
    { title = "Pricing", content = pricingView }


pricingView : Html.Html Msg
pricingView =
    div
        []
        [ div
            [ class "section has-text-centered" ]
            [ h1
                [ class "title is-2" ]
                [ text "Pricing" ]
            , div
                [ class "columns is-centered" ]
                [ div
                    [ class "column is-two-thirds" ]
                    [ div
                        [ class "content" ]
                        [ p [] [ text alphaPricingParagraph1 ]
                        , p [] [ text alphaPricingParagraph2 ]
                        ]
                    ]
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


alphaPricingParagraph1 : String
alphaPricingParagraph1 =
    "Free. Open source."


alphaPricingParagraph2 : String
alphaPricingParagraph2 =
    """VivaDoc is for the community. Lets create something beautiful together."""
