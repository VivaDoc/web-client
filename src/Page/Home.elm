module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| The homepage.
-}

import Api.Core as Core
import Asset
import Browser.Navigation as Nav
import CodeEditor
import Github
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Language
import Ports
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
    let
        initLandingLang =
            Language.JavaScript
    in
    ( { session = session, landingLanguage = initLandingLang }
    , renderLandingCodeEditor initLandingLang
    )



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
                renderLandingPage model.landingLanguage
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


renderLandingPage : Language.Language -> Html Msg
renderLandingPage selectedLanguage =
    div
        [ class "color-bg-grey" ]
        [ div
            [ class "section has-text-centered"
            ]
            [ div
                [ class "title is-2" ]
                [ text "VivaDoc" ]
            , div
                [ class "subtitle is-4" ]
                [ text "Prevent code comments from going out of date by adding checks to your code review pipeline" ]
            ]
        , p
            [ class "has-text-centered" ]
            [ text "select your language, more coming soon!" ]
        , div
            [ class "buttons has-addons is-centered" ]
            [ button
                [ classList
                    [ ( "button is-rounded", True )
                    , ( "is-link", selectedLanguage == Language.C )
                    ]
                , onClick <| SelectLandingLanguage Language.C
                ]
                [ text "C" ]
            , button
                [ classList
                    [ ( "button is-rounded", True )
                    , ( "is-link", selectedLanguage == Language.CPlusPlus )
                    ]
                , onClick <| SelectLandingLanguage Language.CPlusPlus
                ]
                [ text "C++" ]
            , button
                [ classList
                    [ ( "button is-rounded", True )
                    , ( "is-link", selectedLanguage == Language.CSharp )
                    ]
                , onClick <| SelectLandingLanguage Language.CSharp
                ]
                [ text "C#" ]
            , button
                [ classList
                    [ ( "button is-rounded", True )
                    , ( "is-link", selectedLanguage == Language.Go )
                    ]
                , onClick <| SelectLandingLanguage Language.Go
                ]
                [ text "Go" ]
            , button
                [ classList
                    [ ( "button is-rounded", True )
                    , ( "is-link", selectedLanguage == Language.Java )
                    ]
                , onClick <| SelectLandingLanguage Language.Java
                ]
                [ text "Java" ]
            , button
                [ classList
                    [ ( "button is-rounded", True )
                    , ( "is-link", selectedLanguage == Language.JavaScript )
                    ]
                , onClick <| SelectLandingLanguage Language.JavaScript
                ]
                [ text "JavaScript" ]
            , button
                [ classList
                    [ ( "button is-rounded", True )
                    , ( "is-link", selectedLanguage == Language.TypeScript )
                    ]
                , onClick <| SelectLandingLanguage Language.TypeScript
                ]
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
                        [ text "Easily assign ownership to a critical comment" ]
                    , div
                        [ class "landing-code-editor" ]
                        [ CodeEditor.codeEditor "landing-editor" ]
                    ]
                , div
                    [ class "box" ]
                    [ div
                        [ class "title is-5" ]
                        [ text "PRs with changes to this code or comment require approval" ]
                    , img
                        [ Asset.src Asset.prFailed
                        , style "padding-top" "10px"
                        ]
                        []
                    ]
                , div
                    [ class "box" ]
                    [ div
                        [ class "title is-5" ]
                        [ text "The owner quickly verifies if the comment is up-to-date"
                        , img
                            [ Asset.src Asset.approveChange
                            , style "padding-top" "10px"
                            ]
                            []
                        ]
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
    | SelectLandingLanguage Language.Language


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SignUpWithGithub ->
            ( model
            , Nav.load Github.oAuthSignInLink
            )

        SelectLandingLanguage selectedLandingLanguage ->
            ( { model | landingLanguage = selectedLandingLanguage }
            , renderLandingCodeEditor selectedLandingLanguage
            )


renderLandingCodeEditor : Language.Language -> Cmd Msg
renderLandingCodeEditor language =
    let
        renderContent content =
            Ports.renderCodeEditors
                [ { tagId = "landing-editor"
                  , startLineNumber = 1
                  , customLineNumbers = Nothing
                  , redLineRanges = []
                  , greenLineRanges = [ ( 2, 2 ), ( 6, 6 ) ]
                  , content = content
                  , language = Language.toString language
                  }
                ]
    in
    case language of
        Language.C ->
            renderContent
                [ "// Prints hello world to STDOUT and thereby creates a new programmer."
                , "// @VD amilner42 start"
                , "void createProgrammer() {"
                , """  printf("Hello World");"""
                , "}"
                , "// @VD end"
                ]

        Language.CPlusPlus ->
            renderContent
                [ "// Prints hello world to STDOUT and thereby creates a new programmer."
                , "// @VD amilner42 start"
                , "void createProgrammer() {"
                , """  cout << "Hello, World";"""
                , "}"
                , "// @VD end"
                ]

        Language.CSharp ->
            renderContent
                [ "// Prints hello world to STDOUT and thereby creates a new programmer."
                , "// @VD amilner42 start"
                , "public static void CreateProgrammer() {"
                , """  Console.WriteLine("Hello World");"""
                , "}"
                , "// @VD end"
                ]

        Language.Go ->
            renderContent
                [ "// Prints hello world to STDOUT and thereby creates a new programmer."
                , "// @VD amilner42 start"
                , "func createProgrammer() {"
                , """  fmt.Println("Hello World") """
                , "}"
                , "// @VD end"
                ]

        Language.Java ->
            renderContent
                [ "// Prints hello world to STDOUT and thereby creates a new programmer."
                , "// @VD amilner42 start"
                , "public static void createProgrammer() {"
                , """  System.out.println("Hello World");"""
                , "}"
                , "// @VD end"
                ]

        Language.JavaScript ->
            renderContent
                [ "// Prints hello world to the console and thereby creates a new programmer."
                , "// @VD amilner42 start"
                , "const createProgrammer = () => {"
                , """  console.log("Hello World");"""
                , "}"
                , "// @VD end"
                ]

        Language.TypeScript ->
            renderContent
                [ "// Prints hello world to the console and thereby creates a new programmer."
                , "// @VD amilner42 start"
                , "const createProgrammer = (): void => {"
                , """  console.log("Hello World");"""
                , "}"
                , "// @VD end"
                ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession =
    .session
