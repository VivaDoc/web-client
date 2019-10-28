module Page.Documentation exposing (Model, Msg, init, update, view)

import Api.Core as Core
import Bulma
import CodeEditor
import Html exposing (Html, a, aside, dd, div, dl, dt, h1, hr, i, li, p, section, span, text, ul)
import Html.Attributes exposing (class, classList, style)
import Icon
import Language
import Ports
import Route
import Session
import Viewer
import Words


type alias Model =
    { documentationTab : Route.DocumentationTab
    , session : Session.Session
    }


init : Session.Session -> Route.DocumentationTab -> ( Model, Cmd Msg )
init session documentationTab =
    ( { documentationTab = documentationTab, session = session }
    , case documentationTab of
        Route.TagsTab ->
            Ports.renderCodeEditors
                [ docTagsCodeEditor1.renderConfig
                , docTagsCodeEditor2.renderConfig
                ]

        _ ->
            Cmd.none
    )


view : Model -> { title : String, content : Html msg }
view model =
    { title =
        case model.documentationTab of
            Route.InstallationTab ->
                "Installation"

            Route.SupportedLanguagesTab ->
                "Supported Languages"

            Route.OverviewTab ->
                "Overview"

            Route.TagsTab ->
                "Tags"

            Route.OwnershipGroupsTab ->
                "Ownership Groups"
    , content = renderDocumentation model
    }


renderDocumentation : Model -> Html msg
renderDocumentation ({ session, documentationTab } as model) =
    div
        [ class "columns" ]
        [ div
            [ class "column is-one-quarter-desktop is-one-third-tablet" ]
            [ renderSidebar documentationTab ]
        , div
            [ class "column is-three-quarters-desktop is-two-thirds-tablet" ]
            [ section [ class "section" ] [ renderDocTabContent model ] ]
        ]


renderSidebar : Route.DocumentationTab -> Html msg
renderSidebar docTab =
    let
        sidebarLink linkText linkToDocTab maybeSubLinks =
            li
                []
                (a
                    [ if docTab == linkToDocTab then
                        class "is-active"

                      else
                        Route.href <| Route.Documentation linkToDocTab
                    ]
                    [ text linkText ]
                    :: (case maybeSubLinks of
                            Nothing ->
                                []

                            Just subLinks ->
                                [ ul [] subLinks ]
                       )
                )
    in
    div [ class "section" ]
        [ aside
            [ class "box menu" ]
            [ p
                [ class "menu-label" ]
                [ text "General" ]
            , ul
                [ class "menu-list" ]
                [ sidebarLink "overview" Route.OverviewTab Nothing
                , sidebarLink "installation" Route.InstallationTab Nothing
                , sidebarLink "supported languages" Route.SupportedLanguagesTab Nothing
                ]
            , p
                [ class "menu-label" ]
                [ text "API Reference" ]
            , ul
                [ class "menu-list" ]
                [ sidebarLink "documentation tags" Route.TagsTab Nothing
                , sidebarLink "ownership groups" Route.OwnershipGroupsTab Nothing
                ]
            ]
        ]


renderDocTabContent : Model -> Html msg
renderDocTabContent { session, documentationTab } =
    let
        maybeViewer =
            Session.getViewer session
    in
    case documentationTab of
        Route.InstallationTab ->
            renderInstallationTabView maybeViewer

        Route.SupportedLanguagesTab ->
            renderSupportedLanguagesTabView

        Route.OverviewTab ->
            renderOverviewTabView

        Route.TagsTab ->
            renderTagsTabView

        Route.OwnershipGroupsTab ->
            renderOwnershipGroupsTabView


type InstallationStage
    = NotSignedUp
    | SignedUpWithNoRepos
    | SignedUpWithRepos


renderInstallationTabView : Maybe Viewer.Viewer -> Html msg
renderInstallationTabView maybeViewer =
    let
        installationStage =
            case maybeViewer of
                Nothing ->
                    NotSignedUp

                Just viewer ->
                    if
                        Viewer.getRepos viewer
                            |> List.any Core.getRepoAppInstalledStatus
                    then
                        SignedUpWithRepos

                    else
                        SignedUpWithNoRepos

        notSignedUpDt =
            dt
                []
                [ Icon.renderIcon
                    { iconName = "check_box_outline_blank"
                    , optionalAdjacentText = Just ( "Sign up with your github account", Bulma.DarkGrey )
                    , iconSize = Bulma.BulmaSmall
                    , iconColor = Bulma.DarkGrey
                    }
                , dd
                    [ class "content"
                    , style "padding" "10px 25px 25px 25px"
                    ]
                    [ Icon.renderIcon
                        { iconName = "text_information"
                        , optionalAdjacentText =
                            Just
                                ( "Click the sign in button in the top right corner of the navbar."
                                , Bulma.DarkGrey
                                )
                        , iconSize = Bulma.BulmaSmall
                        , iconColor = Bulma.DarkGrey
                        }
                    ]
                ]

        signedUpDt =
            dt
                [ style "margin-bottom" "10px" ]
                [ Icon.renderIcon
                    { iconName = "check_box"
                    , optionalAdjacentText = Just ( "Sign up with your github account", Bulma.LightGrey )
                    , iconSize = Bulma.BulmaSmall
                    , iconColor = Bulma.Success
                    }
                ]

        notInstalledOnRepoDt showExplanation =
            dt
                []
                [ Icon.renderIcon
                    { iconName = "check_box_outline_blank"
                    , optionalAdjacentText =
                        Just
                            ( "Install the app on some of your repos"
                            , if showExplanation then
                                Bulma.DarkGrey

                              else
                                Bulma.LightGrey
                            )
                    , iconSize = Bulma.BulmaSmall
                    , iconColor =
                        if showExplanation then
                            Bulma.DarkGrey

                        else
                            Bulma.LightGrey
                    }
                , dd
                    [ classList
                        [ ( "content", True )
                        , ( "is-hidden", not showExplanation )
                        ]
                    , style "padding" "10px 25px"
                    ]
                    [ Icon.renderIcon
                        { iconName = "text_information"
                        , optionalAdjacentText =
                            Just
                                ( "Go to the home page to install the app on a repository"
                                , Bulma.DarkGrey
                                )
                        , iconSize = Bulma.BulmaSmall
                        , iconColor = Bulma.DarkGrey
                        }
                    ]
                ]

        installedOnRepoDt =
            dt
                []
                [ Icon.renderIcon
                    { iconName = "check_box"
                    , optionalAdjacentText = Just ( "Install the app on some of your repos", Bulma.LightGrey )
                    , iconSize = Bulma.BulmaSmall
                    , iconColor = Bulma.Success
                    }
                ]
    in
    div
        [ class "content" ]
        [ h1
            [ class "title is-2 has-text-vd-base-dark has-text-weight-light"
            ]
            [ text "Installation" ]
        , div
            [ class "content" ]
            [ text "Adding VivaDoc to any GitHub project only takes a few clicks and works completely out of the box." ]
        , dl [ style "padding-left" "20px" ] <|
            case installationStage of
                NotSignedUp ->
                    [ notSignedUpDt
                    , notInstalledOnRepoDt False
                    ]

                SignedUpWithNoRepos ->
                    [ signedUpDt
                    , notInstalledOnRepoDt True
                    ]

                SignedUpWithRepos ->
                    [ signedUpDt
                    , installedOnRepoDt
                    ]
        ]


renderOverviewTabView : Html msg
renderOverviewTabView =
    div [ class "content" ] <|
        [ h1
            [ class "title is-2 has-text-vd-base-dark has-text-weight-light" ]
            [ text "Overview" ]
        , p
            [ class "vd-regular-text" ]
            [ text """VivaDoc is to code comments what CI is to testing. Instead of monitoring tests passing, it's
            monitoring """
            , a [ Route.href <| Route.Documentation Route.TagsTab ] [ text " documentation tags " ]
            , text """ being approved. Instead of preventing bugs from getting into production, you're
            preventing bad code comments from getting into your codebase."""
            ]
        , p
            []
            [ text "When"
            , a [ Route.href <| Route.Documentation Route.TagsTab ] [ text " documentation tags " ]
            , text """ have been modified but have not been approved by their owners, VivaDoc
              assigns a failure status to that commit. Exactly like a CI service, VivaDoc will attach a link on the
              failed commit status, click this to view which tags require approval. Once everything has been reviewed
              and approved, VivaDoc will assign a success status to that commit."""
            ]
        , p
            []
            [ text """Lastly, exactly like a CI service, it is up to the owners of the repository to decide whether a
            pull request can be merged if the documentation has a failing status. VivaDoc defaults to not forcing
            passing documentation because it does not want to get in your way. This way you can easily try VivaDoc
            without worrying about your PRs being blocked due to unverified documentation."""
            ]
        ]


type alias RenderCodeEditorColumnsConfig =
    { renderConfig : Ports.RenderCodeEditorConfig
    , textAboveEditor : String
    , editorSubText : String
    , editorHeight : Int
    }


renderCodeEditorColumns : RenderCodeEditorColumnsConfig -> Html msg
renderCodeEditorColumns { renderConfig, textAboveEditor, editorSubText, editorHeight } =
    div
        []
        [ p [ style "max-width" "700px", style "margin-bottom" "20px" ] [ text textAboveEditor ]
        , div
            [ class "has-code-editor", style "height" <| Words.pixelify editorHeight, style "max-width" "700px" ]
            [ CodeEditor.codeEditor renderConfig.tagId ]
        , div [ class "has-text-grey-light", style "margin-bottom" "40px" ] [ text editorSubText ]
        ]


renderSupportedLanguagesTabView : Html msg
renderSupportedLanguagesTabView =
    div [ class "content" ] <|
        [ h1
            [ class "title is-2 has-text-vd-base-dark has-text-weight-light" ]
            [ text "Supported Languages" ]
        , p
            []
            [ text "VivaDoc is in its early stages and is only able to support the following languages" ]
        , dl
            [ class "has-text-weight-semibold", style "padding-left" "30px" ]
            [ dt [] [ text "C" ]
            , dt [] [ text "C#" ]
            , dt [] [ text "C++" ]
            , dt [] [ text "Go" ]
            , dt [] [ text "Java" ]
            , dt [] [ text "JavaScript" ]
            , dt [] [ text "TypeScript" ]
            ]
        , p [] [ text "More languages will be supported as VivaDoc continues to grow." ]
        , p
            []
            [ text """In the meantime, if your repository uses any of the languages listed above you can immediately get
            started using VivaDoc for files written in those languages. All files in languages that VivaDoc does not
            support will simply be ignored - they will not cause errors."""
            ]
        ]


renderTagsTabView : Html msg
renderTagsTabView =
    div [ class "content" ] <|
        [ h1
            [ class "title is-2 has-text-vd-base-dark has-text-weight-light" ]
            [ text "Documentation Tags" ]
        , p
            []
            [ text """Documentation tags are the core of VivaDoc. They enable you to monitor the documentation
            associated to a chunk of code. If the documentation or the code changes, VivaDoc will require the owners
            to approve it before putting a success status on that commit.""" ]
        , h1
            [ class "title is-5 has-text-vd-base-dark has-text-weight-bold" ]
            [ text "Syntax" ]
        , renderCodeEditorColumns docTagsCodeEditor1
        , renderCodeEditorColumns docTagsCodeEditor2
        , h1
            [ class "title is-5 has-text-vd-base-dark has-text-weight-bold" ]
            [ text "Ownership Groups" ]
        , p
            [ style "margin-bottom" "40px" ]
            [ text "If your team requires more than a single owner for critical documentation tags, simply use "
            , a [ Route.href <| Route.Documentation Route.OwnershipGroupsTab ] [ text "ownership groups" ]
            , text "."
            ]
        ]


docTagsCodeEditor1 : RenderCodeEditorColumnsConfig
docTagsCodeEditor1 =
    { renderConfig =
        { tagId = "doc-tags-1"
        , startLineNumber = 1
        , customLineNumbers = Nothing
        , redLineRanges = []
        , greenLineRanges = []
        , content =
            [ "/* some docs"
            , " * more docs"
            , " * @VD <ownership-groups> start"
            , " */"
            , "... code ..."
            , "// @VD end"
            ]
        , language = Language.toString Language.JavaScript
        }
    , textAboveEditor = "VivaDoc documentation tags always use the following syntax."
    , editorSubText = "multiline comment"
    , editorHeight = 110
    }


docTagsCodeEditor2 : RenderCodeEditorColumnsConfig
docTagsCodeEditor2 =
    { renderConfig =
        { tagId = "doc-tags-2"
        , startLineNumber = 1
        , customLineNumbers = Nothing
        , redLineRanges = []
        , greenLineRanges = []
        , content =
            [ "// some docs"
            , "// more docs"
            , "// @VD <ownership-groups> start"
            , "... code ..."
            , "// @VD end"
            ]
        , language = Language.toString Language.JavaScript
        }
    , textAboveEditor = """VivaDoc will detect and group multiple single line comments that start at the same
    indentation level into a single doc - use whatever you prefer."""
    , editorSubText = "single-line comments"
    , editorHeight = 90
    }


renderOwnershipGroupsTabView : Html msg
renderOwnershipGroupsTabView =
    div [ class "content" ] <|
        [ h1
            [ class "title is-2 has-text-vd-base-dark has-text-weight-light" ]
            [ text "Ownership Groups" ]
        , p
            []
            [ text """The most basic ownership group is a single user, the syntax for that is to just write the github
            username."""
            , div
                [ class "has-text-weight-semibold is-italic", style "margin" "10px 0 0 20px" ]
                [ text "username" ]
            ]
        , p
            []
            [ text """If you want to require multiple people to approve documentation, you can do so by writing all the
            usernames separated by commas. Each comma creates a new ownership group, and approval is always required
            from every ownership group."""
            , div
                [ class "has-text-weight-semibold is-italic", style "margin" "10px 0 0 20px" ]
                [ text "username1,username2" ]
            ]
        , p
            []
            [ text """On the other hand, you may require that either one user or another user approve documentation. The
            syntax for that is to use the unix pipe. This does not create a new ownership group, rather it allows any
            user in the ownership group to approve the documentation on behalf of the group."""
            , div
                [ class "has-text-weight-semibold is-italic", style "margin" "10px 0 0 20px" ]
                [ text "username1|username2" ]
            ]
        , p
            []
            [ text """You can of course combine these two operators. The following example requires approval from
            (username1 or username2) and (username3 or username4). """
            , div
                [ class "has-text-weight-semibold is-italic", style "margin" "10px 0 0 20px" ]
                [ text "username1|username2,username3|username4" ]
            ]
        , p
            []
            [ text """This last example requires approval from username1 or (username2 and username3). Note that
            ownership groups do not allow parenthesis so it must be written in its expanded form."""
            , div
                [ class "has-text-weight-semibold is-italic", style "margin" "10px 0 0 20px" ]
                [ text "username1|username2,username1|username3" ]
            ]
        , p
            []
            [ text """While it is allowed, it is not recommended to create super large ownership groups - this may
            ruin the benefit of the DRI principle and cause team members to think """
            , span [ class "has-text-weight-semibold" ] [ text "someone else will maintain it" ]
            , text """. To avoid this, try to keep groups at a size of 1 - 3 people. Having a single owner is the
            recommended solution for maintaining critical documentation. Having a single owner also has the benefit
            of everyone knowing precisely who to talk to if something in the documentation seems incorrect."""
            ]
        ]


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
