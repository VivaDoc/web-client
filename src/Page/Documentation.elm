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
        Route.CodeExample ->
            Ports.renderCodeEditors
                [ example1a.renderConfig
                , example1b.renderConfig
                , example1c.renderConfig
                , example1d.renderConfig
                , example1e.renderConfig
                , example1f.renderConfig
                ]

        Route.TagsTab ->
            Ports.renderCodeEditors
                [ docTagsCodeEditor1.renderConfig
                , docTagsCodeEditor2.renderConfig
                ]

        Route.FileTagTab ->
            Ports.renderCodeEditors [ fileTagEditor1.renderConfig ]

        Route.LineTagTab ->
            Ports.renderCodeEditors [ lineTagEditor1.renderConfig ]

        Route.BlockTagTab ->
            Ports.renderCodeEditors [ blockTagEditor1.renderConfig ]

        _ ->
            Cmd.none
    )


view : Model -> { title : String, content : Html msg }
view model =
    { title =
        case model.documentationTab of
            Route.InstallationTab ->
                "Installation"

            Route.GettingStartedTab ->
                "Getting Started"

            Route.CodeExample ->
                "Code Example"

            Route.SupportedLanguagesTab ->
                "Supported Languages"

            Route.OverviewTab ->
                "Overview"

            Route.TagsTab ->
                "Tags"

            Route.FileTagTab ->
                "File Tags"

            Route.LineTagTab ->
                "Line Tags"

            Route.BlockTagTab ->
                "Block Tags"

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
                , sidebarLink "getting started" Route.GettingStartedTab <|
                    Just
                        [ sidebarLink "code example" Route.CodeExample Nothing ]
                , sidebarLink "supported languages" Route.SupportedLanguagesTab Nothing
                ]
            , p
                [ class "menu-label" ]
                [ text "API Reference" ]
            , ul
                [ class "menu-list" ]
                [ sidebarLink "documentation tags" Route.TagsTab <|
                    Just
                        [ sidebarLink "file tag" Route.FileTagTab Nothing
                        , sidebarLink "line tag" Route.LineTagTab Nothing
                        , sidebarLink "block tag" Route.BlockTagTab Nothing
                        ]
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

        Route.GettingStartedTab ->
            renderGettingStartedTabView

        Route.CodeExample ->
            renderCodeExampleTabView

        Route.SupportedLanguagesTab ->
            renderSupportedLanguagesTabView

        Route.OverviewTab ->
            renderOverviewTabView

        Route.TagsTab ->
            renderTagsTabView

        Route.FileTagTab ->
            renderFileTagTabView

        Route.LineTagTab ->
            renderLineTagTabView

        Route.BlockTagTab ->
            renderBlockTagTabView

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
    div
        [ class "content" ]
        [ h1
            [ class "title is-2 has-text-vd-base-dark has-text-weight-light" ]
            [ text "Overview" ]
        , span
            [ class "vd-regular-text" ]
            [ text """Conceptually, VivaDoc is built on the """
            , span [ class "has-text-weight-bold" ] [ text """Directly Responsible Individual""" ]
            , text """ principle created and
              currently used at Apple. The principle is rather intuitive, projects will be more successful if each
              component always has someone directly responsible. This model helps keep team members proactive and never
              assuming"""
            , span [ class "has-text-weight-semibold" ] [ text " someone else will handle it." ]
            ]
        , p
            [ class "vd-regular-text", style "margin-top" "30px" ]
            [ text """It has become so common-place in the industry for technical documentation to become outdated
              that it comes as no surprise to developers when half their day is spent mangling a piece of code only to
              find out the documentation for the library they were using was entirely outdated."""
            ]
        , p
            [ class "vd-regular-text", style "margin-top" "30px" ]
            [ text """Enter VivaDoc to establish the new status quo. With VivaDoc, members of your team become directly
              responsible for the documentation of components they manage. Whether it is external documentation for a
              public API or internal
              documentation for a critical library, VivaDoc will make sure that nothing slips by unnoticed.
              Best of all, VivaDoc works entirely out of the box and can be installed in just a few clicks. It can be
              gradually added to any large project so there is no barrier-to-entry. In just a"""
            , span [ class "has-text-weight-semibold" ] [ text " few minutes " ]
            , text """you can assign
              responsibility to the most critical documentation and save your team and your customers countless hours
              of frustration."""
            ]
        , p
            [ class "vd-regular-text", style "margin-top" "30px" ]
            [ text """It is time for all of us as a community to value each other's time and start shipping
              high-quality documentation."""
            ]
        ]


renderGettingStartedTabView : Html msg
renderGettingStartedTabView =
    div [ class "content" ] <|
        [ h1
            [ class "title is-2 has-text-vd-base-dark has-text-weight-light" ]
            [ text "Getting Started" ]
        , p
            [ class "vd-regular-text" ]
            [ text "Once you have installed VivaDoc in a repository, it automatically monitors all"
            , span [ class "has-text-weight-semibold" ] [ text " documentation tags " ]
            , text """ on every open pull request. Everytime a pull request changes, VivaDoc analyzes
            the most recent commit on that pull request."""
            ]
        , p
            []
            [ text """If any documentation tags have been modified but have not been approved by their owners, VivaDoc
            assigns a failure status to that commit. To review documentation tags, click the VivaDoc status link
            displayed on the GitHub pull request. It will direct you to the documentation review page within the VivaDoc
            app. There you will be able to review all documentation tags that require your approval. Once all
            documentation tags have been reviewed and approved, VivaDoc will assign a success status to that commit."""
            ]
        , p
            []
            [ text """It is up to the owners of the repository to decide whether a pull request can be merged if the
            documentation has a failing status. While it is optional,
            it is highly recommended to require a VivaDoc success status to merge a pull request with a production
            branch - fixing broken documentation is not nearly as time consuming or frustrating as stumbling into broken
            documentation unknowingly."""
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


renderCodeExampleTabView : Html msg
renderCodeExampleTabView =
    div [ class "content" ] <|
        [ h1
            [ class "title is-2 has-text-vd-base-dark has-text-weight-light" ]
            [ text "Code Example" ]
        , renderCodeEditorColumns example1a
        , renderCodeEditorColumns example1b
        , renderCodeEditorColumns example1c
        , renderCodeEditorColumns example1d
        , renderCodeEditorColumns example1e
        , renderCodeEditorColumns example1f
        , p
            []
            [ text "This was a fictional example to showcase how simple it is to use VivaDoc." ]
        ]


example1a : RenderCodeEditorColumnsConfig
example1a =
    { renderConfig =
        { tagId = "example-1a"
        , startLineNumber = 200
        , customLineNumbers = Nothing
        , redLineRanges = []
        , greenLineRanges = []
        , content =
            [ "// Place a booking for a given time slot."
            , "// NOTE: Double-booking is permitted."
            , "const bookTimeslot = (locationId: String, timeslot: Timeslot) => { "
            , " ..."
            , "}"
            ]
        , language = Language.toString Language.TypeScript
        }
    , textAboveEditor = "Let us take a look at a single function in an imaginary public booking service API."
    , editorSubText = "The code for the function is omitted."
    , editorHeight = 100
    }


example1b : RenderCodeEditorColumnsConfig
example1b =
    { renderConfig =
        { tagId = "example-1b"
        , startLineNumber = 200
        , customLineNumbers = Nothing
        , redLineRanges = []
        , greenLineRanges = [ ( 202, 202 ), ( 206, 206 ) ]
        , content =
            [ "// Place a booking for a given time slot."
            , "// NOTE: Double-booking is permitted."
            , "// @VD amilner42 block"
            , "const bookTimeslot = (locationId: String, timeslot: Timeslot) => { "
            , " ..."
            , "}"
            , "// @VD end-block"
            ]
        , language = Language.toString Language.TypeScript
        }
    , textAboveEditor = """If I wanted to assign myself, amilner42, to be directly responsible for this documentation
    all I have to do is add a documentation tag.
    In this case I used a block tag to capture the block of code representing the function. Block tags will likely be
    the most common type of documentation tag that you use as they can wrap any chunk of code."""
    , editorSubText = "Diff highlighted in green."
    , editorHeight = 130
    }


example1c : RenderCodeEditorColumnsConfig
example1c =
    { renderConfig =
        { tagId = "example-1c"
        , startLineNumber = 200
        , customLineNumbers = Nothing
        , redLineRanges = []
        , greenLineRanges = [ ( 202, 202 ), ( 206, 206 ) ]
        , content =
            [ "// Place a booking for a given time slot."
            , "// NOTE: Double-booking is permitted."
            , "// @VD amilner42,bderayat block"
            , "const bookTimeslot = (locationId: String, timeslot: Timeslot) => { "
            , " ..."
            , "}"
            , "// @VD end-block"
            ]
        , language = Language.toString Language.TypeScript
        }
    , textAboveEditor = """It may be the case though that you don't want to be the only one directly responsible. If
    you would like to require approval from multiple users you simply list all users separated by commas. In the
    following case, the tag will require approval from both amilner42 and bderayat."""
    , editorSubText = "Diff highlighted in green."
    , editorHeight = 130
    }


example1d : RenderCodeEditorColumnsConfig
example1d =
    { renderConfig =
        { tagId = "example-1d"
        , startLineNumber = 200
        , customLineNumbers = Nothing
        , redLineRanges = []
        , greenLineRanges = [ ( 202, 202 ), ( 206, 206 ) ]
        , content =
            [ "// Place a booking for a given time slot."
            , "// NOTE: Double-booking is permitted."
            , "// @VD amilner42|bderayat block"
            , "const bookTimeslot = (locationId: String, timeslot: Timeslot) => { "
            , " ..."
            , "}"
            , "// @VD end-block"
            ]
        , language = Language.toString Language.TypeScript
        }
    , textAboveEditor = """Had I instead wanted to require approval from either of us, I simply would
    use the following syntax. You can use as many commas and pipes as you need."""
    , editorSubText = "Diff highlighted in green."
    , editorHeight = 130
    }


example1e : RenderCodeEditorColumnsConfig
example1e =
    { renderConfig =
        { tagId = "example-1e"
        , startLineNumber = 200
        , customLineNumbers = Nothing
        , redLineRanges = []
        , greenLineRanges = [ ( 204, 206 ) ]
        , content =
            [ "// Place a booking for a given time slot."
            , "// NOTE: Double-booking is permitted."
            , "// @VD amilner42 block"
            , "const bookTimeslot = (locationId: String, timeslot: Timeslot) => { "
            , "  if (isDoubleBooked(locationId, timeslot)) {"
            , "    throw new BookingError(...);"
            , "  }"
            , "  ..."
            , "}"
            , "// @VD end-block"
            ]
        , language = Language.toString Language.TypeScript
        }
    , textAboveEditor = """Let us now imagine someone changes the code but does not update the documentation to reflect
    this new change."""
    , editorSubText = "Diff highlighted in green."
    , editorHeight = 175
    }


example1f : RenderCodeEditorColumnsConfig
example1f =
    { renderConfig =
        { tagId = "example-1f"
        , startLineNumber = 200
        , customLineNumbers = Just [ Just 200, Nothing, Just 201, Just 202, Just 203, Just 204, Just 205, Just 206, Just 207, Just 208 ]
        , redLineRanges = [ ( 201, 201 ) ]
        , greenLineRanges = []
        , content =
            [ "// Place a booking for a given time slot."
            , "// NOTE: Double-booking is permitted."
            , "// @VD amilner42 block"
            , "const bookTimeslot = (locationId: String, timeslot: Timeslot) => { "
            , "  if (isDoubleBooked(locationId, timeslot)) {"
            , "    throw new BookingError(...);"
            , "  }"
            , "  ..."
            , "}"
            , "// @VD end-block"
            ]
        , language = Language.toString Language.TypeScript
        }
    , textAboveEditor = """Luckily VivaDoc would require my review, and upon VivaDoc showing me the diff and the docs
    it would be rather obvious that the documentation has not been updated properly. I can commit a simple fix preventing
    these docs from frustrating the users consuming our API.
    """
    , editorSubText = "Diff highlighted in red."
    , editorHeight = 175
    }


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
            [ text "VivaDoc permits a single owner for the documentation, such as "
            , span [ class "has-text-weight-semibold" ] [ text "amilner42" ]
            , text """. In addition, it allows ownership groups that allow for more fine-grained control to suit the
            specific needs of your team. You can read about the syntax for ownership groups """
            , a [ Route.href <| Route.Documentation Route.OwnershipGroupsTab ] [ text "here" ]
            , text "."
            ]
        , h1
            [ class "title is-5 has-text-vd-base-dark has-text-weight-bold" ]
            [ text "Tag Types" ]
        , p
            []
            [ text "The tag type must be one of "
            , span [ class "has-text-weight-semibold" ] [ text "file" ]
            , text ", "
            , span [ class "has-text-weight-semibold" ] [ text "line" ]
            , text ", or "
            , span [ class "has-text-weight-semibold" ] [ text "block" ]
            , text "."
            ]
        , dl
            [ style "margin" "10px 0 0 20px" ]
            [ dt
                []
                [ a [ Route.href <| Route.Documentation Route.FileTagTab ] [ text "File Tag" ]
                , dd [] [ text "Associate some documentation to an entire file." ]
                ]
            , dt
                []
                [ a [ Route.href <| Route.Documentation Route.LineTagTab ] [ text "Line Tag" ]
                , dd [] [ text "Associate some documentation to the following line." ]
                ]
            , dt
                []
                [ a [ Route.href <| Route.Documentation Route.BlockTagTab ] [ text "Block Tag" ]
                , dd [] [ text "Associate some documentation to a specified block of code." ]
                ]
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
            , " *"
            , " * @VD <ownership-groups> <tag-type>"
            , " */"
            , "... code ..."
            ]
        , language = Language.toString Language.JavaScript
        }
    , textAboveEditor = "VivaDoc documentation tags always use the following syntax."
    , editorSubText = "multi-line comment"
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
            , "// @VD <ownership-groups> <tag-type>"
            , "... code ..."
            ]
        , language = Language.toString Language.JavaScript
        }
    , textAboveEditor = """VivaDoc will detect and group multiple single line comments that start at the same
    indentation level into a single doc - use whatever you prefer."""
    , editorSubText = "grouped single-line comments"
    , editorHeight = 80
    }


renderFileTagTabView : Html msg
renderFileTagTabView =
    div [ class "content" ] <|
        [ h1
            [ class "title is-2 has-text-vd-base-dark has-text-weight-light" ]
            [ text "File Tag" ]
        , renderCodeEditorColumns fileTagEditor1
        , p
            []
            [ text """You should not use the file tag for monitoring specific chunks of code, the block tag is more
            effective for that situation. The file tag is effective when you have some documentation for the entire
            package or module at the top of the file and you want to make sure it stays relevant as people upgrade the
            package or module.""" ]
        ]


fileTagEditor1 : RenderCodeEditorColumnsConfig
fileTagEditor1 =
    { renderConfig =
        { tagId = "file-tag-1"
        , startLineNumber = 1
        , customLineNumbers = Nothing
        , redLineRanges = []
        , greenLineRanges = []
        , content =
            [ "// module docs"
            , "// module docs"
            , "// @VD amilner42 file"
            , ""
            , "... code ..."
            , ""
            , "... code ..."
            , ""
            , "... code ..."
            ]
        , language = Language.toString Language.JavaScript
        }
    , textAboveEditor = """A file tag will associate the documentation with everything in the entire file.
    Any changes made to any parts of the file will require approval."""
    , editorSubText = "A file tag"
    , editorHeight = 160
    }


renderLineTagTabView : Html msg
renderLineTagTabView =
    div [ class "content" ] <|
        [ h1
            [ class "title is-2 has-text-vd-base-dark has-text-weight-light" ]
            [ text "Line Tag" ]
        , renderCodeEditorColumns lineTagEditor1
        ]


lineTagEditor1 : RenderCodeEditorColumnsConfig
lineTagEditor1 =
    { renderConfig =
        { tagId = "line-tag-1"
        , startLineNumber = 100
        , customLineNumbers = Nothing
        , redLineRanges = []
        , greenLineRanges = []
        , content =
            [ "... code ..."
            , ""
            , "// some docs"
            , "// @VD amilner42 line"
            , "export const WEBPACK_INIT_KEY_VAL = ..."
            , ""
            , "... code ..."
            ]
        , language = Language.toString Language.TypeScript
        }
    , textAboveEditor = """A line tag will associate the documentation with the following line. Any changes
    made to the documentation or the following line will require approval."""
    , editorSubText = "A line tag"
    , editorHeight = 130
    }


renderBlockTagTabView : Html msg
renderBlockTagTabView =
    div [ class "content" ] <|
        [ h1
            [ class "title is-2 has-text-vd-base-dark has-text-weight-light" ]
            [ text "Block Tag" ]
        , renderCodeEditorColumns blockTagEditor1
        , p
            []
            [ text "The block tag should be the standard way you use VivaDoc to monitor documentation." ]
        ]


blockTagEditor1 : RenderCodeEditorColumnsConfig
blockTagEditor1 =
    { renderConfig =
        { tagId = "block-tag-1"
        , startLineNumber = 100
        , customLineNumbers = Nothing
        , redLineRanges = []
        , greenLineRanges = []
        , content =
            [ "... code ..."
            , ""
            , "// some docs"
            , "// @VD amilner42 block"
            , "...code..."
            , "...code..."
            , "...code..."
            , "// @VD end-block"
            ]
        , language = Language.toString Language.TypeScript
        }
    , textAboveEditor = """A block tag will associate the documentation with a block of code. Any changes
    made to the documentation or the block of code will require approval. Unlike the other tags, block tags require
    an end-block VivaDoc annotation in order to specify the end of the block."""
    , editorSubText = "A block tag"
    , editorHeight = 140
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
