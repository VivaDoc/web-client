module Page.AboutUs exposing (Model, Msg(..), init, update, view)

import Asset
import Html exposing (div, h1, img, p, text)
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
    { title = "About Us"
    , content = aboutUsView
    }


aboutUsView : Html.Html Msg
aboutUsView =
    div
        []
        [ div
            [ class "section has-text-centered" ]
            [ div
                [ class "columns is-centered" ]
                [ div
                    [ class "column is-two-thirds" ]
                    [ h1
                        [ class "title is-2" ]
                        [ text "Our Mission" ]
                    , p
                        [ class "content" ]
                        [ text vivaDocMissionStatement ]
                    ]
                ]
            ]
        , div
            [ class "section has-text-centered" ]
            [ h1
                [ class "title is-2"
                , style "padding-bottom" "30px"
                ]
                [ text "Meet the Team" ]
            , div
                [ class "columns" ]
                [ renderUserBio arieBio
                , renderUserBio jakeBio
                ]
            ]
        ]


renderUserBio : BioData -> Html.Html Msg
renderUserBio { name, image, bio } =
    div
        [ class "column is-one-half"
        , style "padding-bottom" "50px"
        ]
        [ img
            [ Asset.src image
            , style "width" "100%"
            , style "max-width" "350px"
            , style "border-radius" "350px"
            ]
            []
        , div
            []
            [ div [ class "title is-4" ] [ text name ]
            , div [ class "content" ] [ text bio ]
            ]
        ]


paddingOneSixthColumn : Html.Html Msg
paddingOneSixthColumn =
    div [ class "column is-one-sixth" ] []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


type alias BioData =
    { name : String
    , image : Asset.Image
    , bio : String
    }


arieBio : BioData
arieBio =
    { name = "Arie Milner"
    , image = Asset.arie
    , bio = """Work experience at Radify and Google. An advocate of practical functional programming and an avid
    theorist and experimentalist on how to make programming suck less. Previous host of the wonderful Elm remote
    meetups. UBC Computer Science graduate."""
    }


jakeBio : BioData
jakeBio =
    { name = "Jake Elward"
    , image = Asset.jake
    , bio = """Former CEO of Kitau, winners of the Pacific Venture Capital Competition. Top design in NVD,
    RBC Get Seeded and Founders Live. Former Product Manager at Grow Technologies. UBC Integrated Engineering
    graduate."""
    }


vivaDocMissionStatement : String
vivaDocMissionStatement =
    """Too many hours have been wasted by hardworking developers on poor documentation. Too many nights have been
    spent with computer glare reflecting off of our tired eyes as we try to figure out why an API is not
    behaving as documented - we did everything the API said was required! Enough is enough. It's time as a community
    that we started valuing each others' time. We all deserve great documentation. But it's not going to happen by
    itself, and that's why VivaDoc is here. VivaDoc is so passionate about good documentation for the community, that
    it will remain free for open source projects...forever. Let's start a documentation revolution together."""
