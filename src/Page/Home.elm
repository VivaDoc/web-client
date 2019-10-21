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
import Language
import Route
import Session exposing (Session)
import Viewer



-- MODEL


type alias Model =
    { session : Session
    , landingLanguage : Language.Language
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, landingLanguage = Language.JavaScript }, Cmd.none )



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
        []
        [ div
            [ class "section has-text-centered"
            ]
            [ div
                [ class "title is-2" ]
                [ text "VivaDoc" ]
            , div
                [ class "subtitle is-4" ]
                [ text "prevent code comments from going out of date by adding checks to your code review pipeline" ]
            ]
        , div
            [ class "buttons has-addons is-centered" ]
            [ button
                [ class "button" ]
                [ text "C" ]
            , button
                [ class "button" ]
                [ text "C++" ]
            , button
                [ class "button" ]
                [ text "C#" ]
            , button
                [ class "button" ]
                [ text "Go" ]
            , button
                [ class "button" ]
                [ text "Java" ]
            , button
                [ class "button" ]
                [ text "JavaScript" ]
            , button
                [ class "button" ]
                [ text "TypeScript" ]
            ]
        , div
            [ class "columns is-centered", style "padding" "1.5rem" ]
            [ div
                [ class "column is-half-desktop is-two-thirds-tablet has-text-centered" ]
                [ div
                    [ class "box" ]
                    [ div
                        [ class "title is-5" ]
                        [ text "Take ownership of a critical comment" ]
                    ]
                , div
                    [ class "box" ]
                    [ div
                        [ class "title is-5" ]
                        [ text "VivaDoc will let you know if that comment needs approval" ]
                    ]
                , div
                    [ class "box" ]
                    [ div
                        [ class "title is-5" ]
                        [ text "Approve the comment " ]
                    ]
                , div
                    [ class "buttons is-centered" ]
                    [ a
                        [ class "button is-medium is-dark"
                        , style "width" "180px"
                        , Route.href <| Route.Documentation Route.OverviewTab
                        ]
                        [ text "Read the docs" ]
                    , button
                        [ class "button is-medium is-info"
                        , style "width" "180px"
                        , onClick SignUpWithGithub
                        ]
                        [ text "Try VivaDoc" ]
                    ]
                ]
            ]
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
