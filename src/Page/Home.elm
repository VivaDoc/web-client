module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| The homepage.
-}

import Api.Core as Core
import Asset
import Browser.Navigation as Nav
import Github
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route
import Session exposing (Session)
import Viewer



-- MODEL


type alias Model =
    { session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    case model.session of
        Session.LoggedIn _ viewer ->
            { title = "Home"
            , content = renderLoggedInHomePage { viewer = viewer }
            }

        Session.Guest _ ->
            { title = "Welcome"
            , content =
                renderLandingPage
            }


renderLoggedInHomePage : { viewer : Viewer.Viewer } -> Html Msg
renderLoggedInHomePage config =
    case (Viewer.getRepos >> Core.getInstalledRepos) config.viewer of
        [] ->
            renderNoRepoPage

        installedRepos ->
            renderHasReposPage installedRepos


renderNoRepoPage : Html Msg
renderNoRepoPage =
    section
        [ class "section is-medium" ]
        [ div
            [ class "container" ]
            [ div
                [ class "columns is-centered is-vcentered" ]
                [ div
                    [ class "column is-half" ]
                    [ h1
                        [ class "title is-1 has-text-centered" ]
                        [ text <| "Welcome to VivaDoc" ]
                    , div
                        [ class "content" ]
                        [ p
                            [ class "has-vd-regular-text has-text-centered" ]
                            [ text """You are one step away from better documentation.""" ]
                        , div
                            [ class "buttons is-centered" ]
                            [ a
                                [ class "button is-primary is-medium"
                                , style "width" "375px"
                                , href Github.installAppOnRepositoriesLink
                                ]
                                [ text "install in select repositories" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


renderHasReposPage : List Core.Repo -> Html.Html Msg
renderHasReposPage installedRepos =
    div
        [ class "section" ]
        [ h1
            [ class "title is-4 has-text-centered" ]
            [ text "Monitored Repositories" ]
        , div
            [ class "columns is-multiline"
            , style "margin" "0px"
            ]
            (List.map renderInstalledRepoLink installedRepos)
        , div
            [ class "buttons is-centered"
            , style "margin-top" "20px"
            ]
            [ a
                [ class "button is-light is-medium"
                , style "width" "375px"
                , href Github.installAppOnRepositoriesLink
                ]
                [ text "add or remove repositories" ]
            ]
        ]


renderInstalledRepoLink : Core.Repo -> Html.Html Msg
renderInstalledRepoLink repo =
    let
        { owner, name } =
            Core.getRepoNameAndOwner repo
    in
    div
        [ class "column is-one-quarter-desktop is-one-third-tablet" ]
        [ div
            [ class "box has-text-centered"
            , style "height" "100px"
            , style "padding" "10px"
            ]
            [ div
                [ class "level is-mobile"
                , style "width" "100%"
                , style "height" "20px"
                , style "margin-bottom" "5px"
                ]
                [ div
                    [ class "level-item level-left" ]
                    [ Github.githubIcon <| Github.githubRepoLink <| Core.getRepoFullName repo ]
                , div
                    [ class "level-item level-right has-text-grey-light" ]
                    [ text owner ]
                ]
            , a
                [ class "has-text-weight-medium single-line-ellipsis"
                , Route.href <| Route.Repo <| Core.getRepoId repo
                ]
                [ text name ]
            ]
        ]


renderLandingPage : Html Msg
renderLandingPage =
    div
        [ class "columns is-multiline"
        , style "height" "100vh"
        , style "padding-top" "30px"
        ]
    <|
        renderLandingPageIconTextCombo
            { text = "In a single line tell VivaDoc to monitor critical documentation."
            , image = Asset.vdLandingIcon1
            }
            ++ renderLandingPageIconTextCombo
                { text = "Sit back as VivaDoc vigilantly monitors documentation across code changes."
                , image = Asset.vdLandingIcon2
                }
            ++ renderLandingPageIconTextCombo
                { text = "Approve or fix outdated documentation when automatically notified by VivaDoc."
                , image = Asset.vdLandingIcon3
                }
            ++ renderLandingButtons


type alias RenderLandingPageIconTextComboConfig =
    { text : String
    , image : Asset.Image
    }


renderLandingPageIconTextCombo : RenderLandingPageIconTextComboConfig -> List (Html msg)
renderLandingPageIconTextCombo config =
    [ div [ class "column is-one-quarter" ] []
    , div
        [ class "column is-one-quarter has-text-centered" ]
        [ img [ Asset.src config.image, style "height" "190px" ] [] ]
    , div
        [ class "column is-one-quarter"
        , style "height" "190px"
        ]
        [ div
            [ class "level level-item has-text-centered-mobile"
            , style "height" "100%"
            , style "padding" "10px"
            ]
            [ text config.text ]
        ]
    , div [ class "column is-one-quarter" ] []
    ]


renderLandingButtons : List (Html Msg)
renderLandingButtons =
    [ div [ class "column is-one-quarter" ] []
    , div
        [ class "column is-half has-text-centered buttons"
        , style "margin-top" "20px"
        ]
        [ a
            [ class "button is-large is-light"
            , Route.href <| Route.Documentation Route.OverviewTab
            , style "min-width" "45%"
            ]
            [ text "Read the docs" ]
        , button
            [ class "button is-large is-primary"
            , onClick SignUpWithGithub
            , style "min-width" "45%"
            ]
            [ text "Sign up with GitHub" ]
        ]
    , div [ class "column is-one-quarter" ] []
    ]



-- UPDATE


type Msg
    = SignUpWithGithub


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SignUpWithGithub ->
            ( model
            , Nav.load Github.oAuthSignInLink
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession =
    .session
