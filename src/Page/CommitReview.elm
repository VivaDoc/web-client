module Page.CommitReview exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api.Api as Api
import Api.Core as Core
import Api.Errors.GetCommitReview as GcrError
import Api.Errors.PostUserAssessments as PuaError
import Api.Responses.GetCommitReview as GcrResponse
import Api.Responses.PostUserAssessments as PuaResponse
import Bulma
import CodeEditor
import CommitReview
import Github
import Html exposing (Html, a, button, dd, div, dl, dt, h1, hr, i, img, li, ol, p, progress, section, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, classList, disabled, href, max, style, value)
import Html.Events exposing (onClick)
import Icon
import Language
import Loading
import OwnerGroup as OG
import Ports
import Progress
import RemoteData
import Route
import Session exposing (Session)
import Set
import UserAssessment as UA
import Viewer
import Words



-- MODEL


type alias Model =
    { session : Session.Session
    , repoId : Int
    , prNumber : Int
    , commitId : String
    , commitReview :
        RemoteData.RemoteData (Core.HttpError GcrError.GetCommitReviewError) GcrResponse.CommitReviewResponse
    , authorFilter : CommitReview.AuthorFilter
    , reviewStateFilter : CommitReview.ReviewStateFilter
    , modalClosed : Bool
    , submitDocReviewState : SubmitDocReviewState
    }


type SubmitDocReviewState
    = NotAsked
    | Loading
    | FullFailure (Core.HttpError PuaError.PostUserAssessmentsError)
    | PartialFailure


init : Session -> Int -> Int -> String -> ( Model, Cmd Msg )
init session repoId prNumber commitId =
    let
        model =
            { session = session
            , repoId = repoId
            , prNumber = prNumber
            , commitId = commitId
            , commitReview = RemoteData.NotAsked
            , reviewStateFilter = CommitReview.AnyReviewState
            , authorFilter = CommitReview.AnyAuthor
            , modalClosed = False
            , submitDocReviewState = NotAsked
            }
    in
    case session of
        Session.Guest _ ->
            ( model, Cmd.none )

        Session.LoggedIn _ viewer ->
            let
                -- TODO
                -- Check that the viewer has that repoId listed, they might have had their permissions withdrawn
                -- in the meantime but this is a minimum client-side check.
                userHasAccessToThisRepo =
                    True
            in
            if userHasAccessToThisRepo then
                ( { model | commitReview = RemoteData.Loading }
                , Api.getCommitReview repoId prNumber commitId CompletedGetCommitReview
                )

            else
                ( model, Cmd.none )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Review"
    , content =
        case model.session of
            Session.Guest _ ->
                div [ class "section" ]
                    [ text "You need to be logged in to view this page..." ]

            Session.LoggedIn _ viewer ->
                case model.commitReview of
                    RemoteData.NotAsked ->
                        div [ class "section" ]
                            [ text "Logged in... " ]

                    RemoteData.Loading ->
                        Loading.renderLoadingScreen "loading commit review"

                    RemoteData.Failure err ->
                        div [ class "section" ] <| renderGetCommitReviewErrorModal err

                    RemoteData.Success { headCommitId, responseType } ->
                        if headCommitId /= model.commitId && not model.modalClosed then
                            div [ class "section" ] <|
                                renderHeadUpdatedModal
                                    responseType
                                    """This commit is stale! You can continue to browse to see what was previosly
                                    approved/rejected but if you would like to make changes you must go to the most
                                    recent commit in the PR.
                                    """
                                    (Route.CommitReview model.repoId model.prNumber headCommitId)

                        else
                            case responseType of
                                GcrResponse.Pending forCommits ->
                                    div [ class "section" ] <|
                                        renderPendingAnalysisPane model.commitId forCommits

                                GcrResponse.AnalysisFailed withReason ->
                                    div [ class "section" ] <|
                                        renderAnalysisFailedPane withReason

                                GcrResponse.Complete commitReview ->
                                    renderCommitReview
                                        { username = Viewer.getUsername viewer
                                        , authorFilter = model.authorFilter
                                        , reviewStateFilter = model.reviewStateFilter
                                        , isCommitStale = model.commitId /= headCommitId
                                        , submitDocReviewState = model.submitDocReviewState
                                        }
                                        commitReview
    }


renderCommitReview :
    { username : String
    , isCommitStale : Bool
    , authorFilter : CommitReview.AuthorFilter
    , reviewStateFilter : CommitReview.ReviewStateFilter
    , submitDocReviewState : SubmitDocReviewState
    }
    -> CommitReview.CommitReview
    -> Html.Html Msg
renderCommitReview config commitReview =
    let
        tagCountBreakdown =
            CommitReview.getTagCountBreakdownForFiles commitReview.fileReviews

        noTagsInReviewMessageSection =
            div [ class "section is-large" ]
                [ div
                    [ class "title has-text-centered" ]
                    [ text "No documentation needs review" ]
                , div
                    [ class "subtitle has-text-centered" ]
                    [ text "Let's call it a day and grab a beer..." ]
                ]

        statusSection =
            renderStatusSection
                { totalTagCount = tagCountBreakdown.totalCount
                , approvedTagCount = tagCountBreakdown.approvedCount
                , rejectedTagCount = tagCountBreakdown.rejectedCount
                , unresolvedTagCount = tagCountBreakdown.unresolvedCount
                }

        reviewsSection =
            renderReviewSection
                { username = config.username
                , authorFilter = config.authorFilter
                , reviewStateFilter = config.reviewStateFilter
                , isCommitStale = config.isCommitStale
                , submitDocReviewState = config.submitDocReviewState
                }
                commitReview
    in
    if tagCountBreakdown.totalCount == 0 then
        noTagsInReviewMessageSection

    else
        -- NOTE We always render the file reviews and hide them with "is-hidden" because of the nature of elm's VDOM
        -- not being aware of the code editors. This prevents us from calling to the port every time as they stay
        -- rendered but hidden.
        div [] [ statusSection, reviewsSection ]



-- TODO figure out where PR link goes
-- , div
--     [ class "level"
--     , style "margin-top" "10px"
--     ]
--     [ a
--         [ class "level-left"
--         , href <| Github.githubPullRequestLink config.repoFullName config.pullRequestNumber
--         ]
--         [ span [ class "level-item" ] [ text "View PR on GitHub" ] ]
--     ]


type alias RenderStatusSectionConfig =
    { totalTagCount : Int
    , approvedTagCount : Int
    , rejectedTagCount : Int
    , unresolvedTagCount : Int
    }


renderStatusSection : RenderStatusSectionConfig -> Html Msg
renderStatusSection config =
    let
        progress =
            Progress.progress
                { height = "5px"
                , width = "100%"
                , bars =
                    [ { color = Progress.Success
                      , widthPercent = toFloat config.approvedTagCount / toFloat config.totalTagCount * 100
                      , text = Nothing
                      }
                    , { color = Progress.Danger
                      , widthPercent = toFloat config.rejectedTagCount / toFloat config.totalTagCount * 100
                      , text = Nothing
                      }
                    ]
                }
    in
    div [ class "section is-small has-text-centered-mobile" ]
        [ div
            [ class "box" ]
            [ h1 [ class "title is-4" ] [ text "Completion Status" ]
            , progress
            , subProgressText
                { totalTagCount = config.totalTagCount
                , approvedTagCount = config.approvedTagCount
                , rejectedTagCount = config.rejectedTagCount
                , unresolvedTagCount = config.unresolvedTagCount
                }
            ]
        ]


subProgressText :
    { totalTagCount : Int
    , approvedTagCount : Int
    , rejectedTagCount : Int
    , unresolvedTagCount : Int
    }
    -> Html msg
subProgressText { totalTagCount, approvedTagCount, rejectedTagCount, unresolvedTagCount } =
    let
        approvedSubText =
            span
                [ classList
                    [ ( "level-item", True )
                    , ( "is-hidden", approvedTagCount == 0 )
                    ]
                ]
                [ text <|
                    Words.singularAndPlural
                        { count = approvedTagCount
                        , singular = "1 approved."
                        , pluralPrefix = ""
                        , pluralSuffix = " approved."
                        }
                ]

        rejectedSubText =
            span
                [ classList
                    [ ( "level-item", True )
                    , ( "is-hidden", rejectedTagCount == 0 )
                    ]
                ]
                [ text <|
                    Words.singularAndPlural
                        { count = rejectedTagCount
                        , singular = "1 rejected."
                        , pluralPrefix = ""
                        , pluralSuffix = " rejected."
                        }
                ]

        unresolvedSubText =
            span
                [ classList
                    [ ( "level-item", True )
                    , ( "is-hidden", unresolvedTagCount == 0 )
                    ]
                ]
                [ text <|
                    Words.singularAndPlural
                        { count = unresolvedTagCount
                        , singular = "1 unresolved."
                        , pluralPrefix = ""
                        , pluralSuffix = " unresolved."
                        }
                ]
    in
    div [ class "level", style "margin-top" "10px" ] <|
        if unresolvedTagCount == totalTagCount then
            [ span
                [ class "level-item" ]
                [ text <| "No documentation has been reviewed." ]
            ]

        else if approvedTagCount == totalTagCount then
            [ span
                [ class "level-item" ]
                [ text <| "All documentation has been approved." ]
            ]

        else if rejectedTagCount == totalTagCount then
            [ span
                [ class "level-item" ]
                [ text <| "All documentation has been rejected." ]
            ]

        else
            [ approvedSubText, rejectedSubText, unresolvedSubText ]


type alias RenderReviewSectionConfig =
    { username : String
    , authorFilter : CommitReview.AuthorFilter
    , reviewStateFilter : CommitReview.ReviewStateFilter
    , isCommitStale : Bool
    , submitDocReviewState : SubmitDocReviewState
    }


renderReviewSection : RenderReviewSectionConfig -> CommitReview.CommitReview -> Html Msg
renderReviewSection config commitReview =
    let
        displayingReviewsCount =
            CommitReview.countVisibleReviewsAndTags commitReview.fileReviews

        docReviewTagIds =
            CommitReview.getTagIdsInDocReview commitReview

        commitReviewHeader =
            renderCommitReviewHeader
                { username = config.username
                , authorFilter = config.authorFilter
                , reviewStateFilter = config.reviewStateFilter
                , docReviewTagIds = docReviewTagIds
                , submitDocReviewState = config.submitDocReviewState
                }

        noReviewsDisplayedText =
            if displayingReviewsCount == 0 then
                renderNoReviewsDisplayedText
                    { username = config.username
                    , authorFilter = config.authorFilter
                    , reviewStateFilter = config.reviewStateFilter
                    }

            else
                div [ class "is-hidden" ] []

        fileReviews =
            commitReview.fileReviews
                |> List.map
                    (renderFileReview
                        { username = config.username
                        , isCommitStale = config.isCommitStale
                        }
                    )
    in
    div [ class "section is-small", style "padding-top" "20px" ] <|
        [ commitReviewHeader
        , noReviewsDisplayedText
        ]
            ++ fileReviews


type alias RenderNoReviewsDisplayedTextConfig =
    { username : String
    , authorFilter : CommitReview.AuthorFilter
    , reviewStateFilter : CommitReview.ReviewStateFilter
    }


renderNoReviewsDisplayedText : RenderNoReviewsDisplayedTextConfig -> Html Msg
renderNoReviewsDisplayedText config =
    let
        tagTextFromReviewStateFilter reviewStateFilter =
            case reviewStateFilter of
                CommitReview.Unresolved ->
                    "unresolved tags"

                CommitReview.Resolved ->
                    "resolved tags"

                CommitReview.AnyReviewState ->
                    "tags"

        authorOwnershipText author =
            if config.username == author then
                "your"

            else
                author ++ "'s"

        hasAuthorFilterText author reviewStateFilter =
            "There are 0 "
                ++ tagTextFromReviewStateFilter reviewStateFilter
                ++ " under "
                ++ "your ownership."

        notAuthorFilterText author reviewStateFilter =
            "There are 0 "
                ++ tagTextFromReviewStateFilter reviewStateFilter
                ++ " outside of "
                ++ " your ownership."
    in
    div
        [ class "section is-small has-text-centered" ]
        [ text <|
            case ( config.authorFilter, config.reviewStateFilter ) of
                ( CommitReview.HasAuthor author, reviewStateFilter ) ->
                    hasAuthorFilterText author reviewStateFilter

                ( CommitReview.AnyAuthor, CommitReview.Unresolved ) ->
                    "No unresolved tags remain."

                -- Should never happen because `renderNoReviewsDisplayedText` won't be called in this case.
                ( CommitReview.AnyAuthor, CommitReview.AnyReviewState ) ->
                    ""

                ( CommitReview.AnyAuthor, CommitReview.Resolved ) ->
                    "No tags have been resolved yet."

                ( CommitReview.NotAuthor author, reviewStateFilter ) ->
                    notAuthorFilterText author reviewStateFilter
        ]


type alias RenderCommitReviewHeaderConfig =
    { username : String
    , authorFilter : CommitReview.AuthorFilter
    , reviewStateFilter : CommitReview.ReviewStateFilter
    , docReviewTagIds : CommitReview.DocReviewTagIds
    , submitDocReviewState : SubmitDocReviewState
    }


renderCommitReviewHeader : RenderCommitReviewHeaderConfig -> Html.Html Msg
renderCommitReviewHeader config =
    div
        []
        [ h1
            [ class "title has-text-centered is-1", style "margin-bottom" "40px" ]
            [ text "Review" ]
        , div
            [ class "level" ]
            [ div
                [ class "level-left" ]
                [ div
                    [ class "level-item" ]
                    [ div
                        [ class "buttons has-addons vd-toggle-buttons" ]
                        [ button
                            [ style "width" "120px"
                            , classList
                                [ ( "button is-rounded", True )
                                , ( "is-active", config.authorFilter == CommitReview.NotAuthor config.username )
                                ]
                            , onClick <| SetAuthorFilter <| CommitReview.NotAuthor config.username
                            ]
                            [ text "not yours" ]
                        , button
                            [ style "width" "0px"
                            , classList
                                [ ( "button is-rounded", True )
                                , ( "is-active", config.authorFilter == CommitReview.AnyAuthor )
                                ]
                            , onClick <| SetAuthorFilter CommitReview.AnyAuthor
                            ]
                            [ text "-" ]
                        , button
                            [ style "width" "120px"
                            , classList
                                [ ( "button is-rounded", True )
                                , ( "is-active", config.authorFilter == CommitReview.HasAuthor config.username )
                                ]
                            , onClick <| SetAuthorFilter <| CommitReview.HasAuthor config.username
                            ]
                            [ text "yours" ]
                        ]
                    ]
                , div
                    [ class "level-item" ]
                    [ div
                        [ class "buttons has-addons vd-toggle-buttons" ]
                        [ button
                            [ style "width" "120px"
                            , classList
                                [ ( "button is-rounded", True )
                                , ( "is-active", config.reviewStateFilter == CommitReview.Unresolved )
                                ]
                            , onClick <| SetReviewStateFilter CommitReview.Unresolved
                            ]
                            [ text "unresolved" ]
                        , button
                            [ style "width" "0px"
                            , classList
                                [ ( "button is-rounded", True )
                                , ( "is-active", config.reviewStateFilter == CommitReview.AnyReviewState )
                                ]
                            , onClick <| SetReviewStateFilter CommitReview.AnyReviewState
                            ]
                            [ text "-" ]
                        , button
                            [ style "width" "120px"
                            , classList
                                [ ( "button is-rounded", True )
                                , ( "is-active", config.reviewStateFilter == CommitReview.Resolved )
                                ]
                            , onClick <| SetReviewStateFilter CommitReview.Resolved
                            ]
                            [ text "resolved" ]
                        ]
                    ]
                ]
            , div
                [ class "level-right" ]
                [ div
                    [ class "level-item" ]
                    [ renderSubmitReviewButton
                        { docReviewTagIds = config.docReviewTagIds
                        , username = config.username
                        , submitDocReviewState = config.submitDocReviewState
                        }
                    ]
                ]
            ]
        ]


type alias RenderSubmitReviewButtonConfig =
    { docReviewTagIds : CommitReview.DocReviewTagIds
    , username : String
    , submitDocReviewState : SubmitDocReviewState
    }


renderSubmitReviewButton : RenderSubmitReviewButtonConfig -> Html Msg
renderSubmitReviewButton config =
    case config.submitDocReviewState of
        NotAsked ->
            button
                [ class "button is-success is-medium is-rounded"
                , onClick <| SubmitDocReview config.username config.docReviewTagIds
                , disabled <| not <| CommitReview.hasTagsInDocReview config.docReviewTagIds
                ]
                [ text "Submit" ]

        Loading ->
            button
                [ class "button is-success is-medium is-rounded is-loading" ]
                [ text "Submit" ]

        FullFailure _ ->
            button
                [ class "button is-danger is-medium is-rounded"
                , disabled True
                ]
                [ text "No tags were updated." ]

        PartialFailure ->
            div
                [ class "section", style "margin-top" "-50px" ]
                [ button
                    [ class "button is-danger is-medium is-rounded"
                    , disabled True
                    ]
                    [ text "Some tags failed to update." ]
                ]


type alias RenderFileReviewConfig =
    { username : String
    , isCommitStale : Bool
    }


renderFileReview : RenderFileReviewConfig -> CommitReview.FileReview -> Html.Html Msg
renderFileReview config fileReview =
    let
        fileReviewHeader =
            renderFileReviewHeader fileReview.currentFilePath fileReview.fileReviewType

        fileReviewTags =
            case fileReview.fileReviewType of
                CommitReview.NewFileReview tags ->
                    renderTags
                        config.username
                        "This tag is being added to a new file."
                        config.isCommitStale
                        fileReview.currentLanguage
                        tags

                CommitReview.DeletedFileReview tags ->
                    renderTags
                        config.username
                        "This tag is being removed inside a deleted file."
                        config.isCommitStale
                        fileReview.currentLanguage
                        tags

                CommitReview.ModifiedFileReview reviews ->
                    renderReviews
                        config.username
                        config.isCommitStale
                        fileReview.currentLanguage
                        reviews

                CommitReview.RenamedFileReview _ _ reviews ->
                    renderReviews
                        config.username
                        config.isCommitStale
                        fileReview.currentLanguage
                        reviews
    in
    div
        [ classList
            [ ( "columns is-multiline", True )
            , ( "is-hidden", fileReview.isHidden )
            ]
        ]
        (fileReviewHeader :: fileReviewTags)


renderFileReviewHeader : String -> CommitReview.FileReviewType -> Html.Html Msg
renderFileReviewHeader currentFilePath fileReviewType =
    div
        [ class "column is-full "
        , style "margin-top" "50px"
        ]
        [ div
            [ style "padding" "10px"
            , style "border-bottom" "0.5px solid #DBDBDB"
            ]
            [ span
                [ class "title is-3"
                , style "font-weight" "400"
                ]
                [ text currentFilePath ]
            , span
                [ class "subtitle is-6"
                , style "margin-left" "10px"
                , style "color" "#ABABAB"
                ]
                [ text <|
                    case fileReviewType of
                        CommitReview.NewFileReview _ ->
                            "new file"

                        CommitReview.ModifiedFileReview _ ->
                            "modified file"

                        CommitReview.DeletedFileReview _ ->
                            "deleted file"

                        CommitReview.RenamedFileReview _ _ _ ->
                            "renamed file"
                ]
            ]
        ]


renderTags :
    String
    -> String
    -> Bool
    -> Language.Language
    -> List CommitReview.Tag
    -> List (Html.Html Msg)
renderTags username description isCommitStale language tags =
    List.map
        (renderTagOrReview
            { username = username
            , description = description
            , isCommitStale = isCommitStale
            , maybeReview = Nothing
            , language = language
            }
        )
        tags


renderReviews :
    String
    -> Bool
    -> Language.Language
    -> List CommitReview.Review
    -> List (Html.Html Msg)
renderReviews username isCommitStale language reviews =
    List.map
        (\review ->
            renderTagOrReview
                { username = username
                , description =
                    case review.reviewType of
                        CommitReview.ReviewNewTag _ ->
                            "This tag is being added to an existing file."

                        CommitReview.ReviewDeletedTag _ ->
                            "This tag is being deleted from an existing file."

                        CommitReview.ReviewModifiedTag _ ->
                            "This tag is being modified."
                , isCommitStale = isCommitStale
                , maybeReview = Just review
                , language = language
                }
                review.tag
        )
        reviews


renderTagOrReview :
    { username : String
    , description : String
    , isCommitStale : Bool
    , maybeReview : Maybe CommitReview.Review
    , language : Language.Language
    }
    -> CommitReview.Tag
    -> Html.Html Msg
renderTagOrReview config tag =
    let
        editor =
            renderEditorColumn
                { tagId = tag.tagId
                , maybeReview = config.maybeReview
                , language = config.language
                }

        editorInfoBox =
            div
                [ class "column is-4", style "margin-bottom" "30px" ]
                [ renderEditorInfoBox
                    { tag = tag
                    , description = config.description
                    , username = config.username
                    , isCommitStale = config.isCommitStale
                    , maybeReview = config.maybeReview
                    , language = config.language
                    }
                ]
    in
    div
        [ classList
            [ ( "column is-full", True )
            , ( "is-hidden", tag.isHidden )
            ]
        ]
        [ div
            [ class "columns" ]
            [ editor, editorInfoBox ]
        ]


type alias RenderEditorColumnConfig =
    { tagId : String
    , maybeReview : Maybe CommitReview.Review
    , language : Language.Language
    }


renderEditorColumn : RenderEditorColumnConfig -> Html Msg
renderEditorColumn config =
    let
        maybeShowDiffValAndToggleMsg : Maybe ( Bool, Msg )
        maybeShowDiffValAndToggleMsg =
            config.maybeReview
                |> Maybe.andThen
                    (\review ->
                        case review.reviewType of
                            CommitReview.ReviewNewTag showingDiff ->
                                Just ( showingDiff, ToggleShowAlteredLines config.language review )

                            CommitReview.ReviewDeletedTag _ ->
                                Nothing

                            CommitReview.ReviewModifiedTag showingDiff ->
                                Just ( showingDiff, ToggleShowAlteredLines config.language review )
                    )

        hasCodeEditorDiv =
            div
                [ class "has-code-editor" ]
                [ CodeEditor.codeEditor config.tagId ]

        showDiffText showDiffVal =
            if showDiffVal then
                "hide diff"

            else
                "show diff"
    in
    div [ class "column is-8" ] <|
        case maybeShowDiffValAndToggleMsg of
            Nothing ->
                [ hasCodeEditorDiv ]

            Just ( showDiffVal, toggleMsg ) ->
                [ div
                    [ class "buttons is-right buttons-above-editor-mobile" ]
                    [ button
                        [ class "button is-outlined is-medium is-link right-button"
                        , onClick <| toggleMsg
                        ]
                        [ text <| showDiffText showDiffVal ]
                    ]
                , hasCodeEditorDiv
                , div
                    [ class "has-text-right is-hidden-mobile sub-editor-action-word"
                    , onClick <| toggleMsg
                    ]
                    [ text <| showDiffText showDiffVal ]
                ]


type alias RenderEditorInfoBoxConfig =
    { tag : CommitReview.Tag
    , description : String
    , username : String
    , isCommitStale : Bool
    , maybeReview : Maybe CommitReview.Review
    , language : Language.Language
    }


renderEditorInfoBox : RenderEditorInfoBoxConfig -> Html Msg
renderEditorInfoBox config =
    let
        ownerGroups =
            renderOwnerGroupsForTag
                config.tag.ownerGroups
                config.tag.userAssessments

        description =
            p [ class "has-text-grey", style "margin-left" "0" ] [ text config.description ]

        tagActionButtons =
            renderTagActionButtons
                { username = config.username
                , tag = config.tag
                , isCommitStale = config.isCommitStale
                , maybeReview = config.maybeReview
                , language = config.language
                }
    in
    div
        [ class "box is-light-grey has-negative-top-margin-on-mobile"
        , style "width" "100%"
        , style "border-radius" "0"
        , style "margin-bottom" "20px"
        ]
        [ div
            [ class "content is-small" ]
            [ renderInfoBoxTopBar { tag = config.tag }
            , description
            , dl [ style "margin" "15px 0 10px 0" ] ownerGroups
            , tagActionButtons
            ]
        ]


type alias RenderInfoBoxTopBarConfig =
    { tag : CommitReview.Tag }


renderInfoBoxTopBar : RenderInfoBoxTopBarConfig -> Html msg
renderInfoBoxTopBar config =
    let
        tagTypeTitle =
            div [ class "level-left" ]
                [ p
                    [ class "title is-6", style "font-weight" "400" ]
                    [ text <| CommitReview.readableTagType config.tag.tagType ]
                ]

        approvalStateTitle =
            case config.tag.approvedState of
                CommitReview.Neutral ->
                    div
                        [ class "level-right has-text-grey" ]
                        [ text "Unresolved" ]

                CommitReview.InDocReview assessmentType ->
                    div
                        [ class "level-right has-text-grey" ]
                        [ text <| "Marked as " ++ UA.prettyPrintAssessmentType assessmentType ]

                CommitReview.InDocReviewBeingSubmitted assessmentType ->
                    div
                        [ class "level-right has-text-grey" ]
                        [ text <| "Requesting..." ]

                CommitReview.NonNeutral assessmentType ->
                    div
                        [ classList
                            [ ( "level-right", True )
                            , ( "has-text-success", UA.isApproved assessmentType )
                            , ( "has-text-danger", UA.isRejected assessmentType )
                            ]
                        ]
                        [ text <| UA.prettyPrintAssessmentTypeWithCapital assessmentType ]

                CommitReview.RequestFailed err ->
                    div [ class "is-hidden" ] []
    in
    div [ class "level is-mobile", style "margin-bottom" "15px" ]
        [ tagTypeTitle, approvalStateTitle ]


renderOwnerGroupsForTag : List OG.OwnerGroup -> List UA.UserAssessment -> List (Html msg)
renderOwnerGroupsForTag ownerGroups userAssessments =
    let
        renderGroup group =
            let
                isApprovedGroup =
                    List.any
                        (\owner ->
                            List.any
                                (UA.isAll [ .assessmentType >> UA.isApproved, UA.isForUser owner ])
                                userAssessments
                        )
                        group

                isRejectedGroup =
                    List.any
                        (\owner ->
                            List.any
                                (UA.isAll [ .assessmentType >> UA.isRejected, UA.isForUser owner ])
                                userAssessments
                        )
                        group
            in
            dd
                [ class "level is-mobile"
                , style "overflow-x" "auto"
                , style "overflow-y" "hidden"
                , style "margin" "5px 0 5px 10px"
                ]
                [ div [ class "level-left", style "margin-left" "1px" ] <|
                    [ span [ class "icon is-small" ]
                        [ i
                            [ classList
                                [ ( "material-icons", True )
                                , ( "has-text-grey", not isApprovedGroup && not isRejectedGroup )
                                , ( "has-text-success", isApprovedGroup )
                                , ( "has-text-danger", isRejectedGroup )
                                ]
                            ]
                            [ text <|
                                case ( isRejectedGroup, isApprovedGroup ) of
                                    ( True, False ) ->
                                        "indeterminate_check_box"

                                    ( True, True ) ->
                                        "indeterminate_check_box"

                                    ( False, True ) ->
                                        "check_box"

                                    ( False, False ) ->
                                        "check_box_outline_blank"
                            ]
                        ]
                    , span [ style "white-space" "pre" ] [ text " " ]
                    ]
                        ++ List.map renderOwner group
                ]

        renderOwner owner =
            let
                isSuccessOwner =
                    List.any (UA.isAll [ UA.isForUser owner, .assessmentType >> UA.isApproved ]) userAssessments

                isDangerOwner =
                    List.any (UA.isAll [ UA.isForUser owner, .assessmentType >> UA.isRejected ]) userAssessments
            in
            span
                [ style "white-space" "pre"
                , class <|
                    case ( isSuccessOwner, isDangerOwner ) of
                        ( True, _ ) ->
                            "has-text-success"

                        ( _, True ) ->
                            "has-text-danger"

                        _ ->
                            "has-text-grey"
                ]
                [ text <| "  " ++ owner ]
    in
    List.map renderGroup ownerGroups


type alias RenderTagActionButtonsConfig =
    { username : String
    , tag : CommitReview.Tag
    , isCommitStale : Bool
    , maybeReview : Maybe CommitReview.Review
    , language : Language.Language
    }


renderTagActionButtons : RenderTagActionButtonsConfig -> Html Msg
renderTagActionButtons config =
    if
        (not <| OG.isUserInAnyGroup config.username config.tag.ownerGroups)
            || List.any (UA.isForUser config.username) config.tag.userAssessments
    then
        div [ class "is-hidden" ] []

    else
        div
            [ class "buttons is-centered is-ancestor"
            , style "margin-top" "20px"
            ]
        <|
            case config.tag.approvedState of
                CommitReview.Neutral ->
                    [ button
                        [ class "button is-danger has-text-white tile"
                        , style "border-radius" "2px"
                        , onClick <| AddToDocReview UA.Rejected config.tag.tagId
                        , disabled <| config.isCommitStale
                        ]
                        [ text "requires fix" ]
                    , button
                        [ class "button is-success has-text-white tile"
                        , style "border-radius" "2px"
                        , onClick <| AddToDocReview UA.Approved config.tag.tagId
                        , disabled <| config.isCommitStale
                        ]
                        [ text "looks good" ]
                    ]

                CommitReview.InDocReview assessmentType ->
                    [ button
                        [ class "button is-fullwidth is-light"
                        , style "border-radius" "2px"
                        , onClick <| RemoveFromDocReview config.tag.tagId
                        , disabled <| config.isCommitStale
                        ]
                        [ text "undo" ]
                    ]

                CommitReview.InDocReviewBeingSubmitted assessmentType ->
                    [ button
                        [ class "button is-fullwidth is-outlined is-loading" ]
                        []
                    ]

                CommitReview.NonNeutral assessmentType ->
                    []

                -- TODO handle error better?
                CommitReview.RequestFailed err ->
                    [ button
                        [ class "button is-danger is-fullwidth"
                        , disabled True
                        ]
                        [ text "Internal Error" ]
                    ]


renderHeadUpdatedModal : GcrResponse.CommitReviewResponseType -> String -> Route.Route -> List (Html.Html Msg)
renderHeadUpdatedModal gcrResponseType modalText headCommitRoute =
    [ div
        [ class "modal is-active" ]
        [ div [ class "modal-background" ] []
        , div
            [ class "modal-card" ]
            [ section
                [ class "modal-card-head" ]
                [ Icon.renderIcon
                    { iconName = "warning"
                    , optionalAdjacentText = Just ( "warning", Bulma.DarkGrey )
                    , iconSize = Bulma.BulmaMedium
                    , iconColor = Bulma.Warning
                    }
                ]
            , section
                [ class "modal-card-body" ]
                [ div
                    [ class "content has-text-centered" ]
                    [ text modalText ]
                , hr [ class "styled-hr" ] []
                , div
                    [ class "buttons are-large is-centered" ]
                    [ button
                        [ class "button is-light tile"
                        , onClick <| SetModalClosed True gcrResponseType
                        ]
                        [ text "browse stale commit" ]
                    , a
                        [ class "button is-link tile"
                        , Route.href headCommitRoute
                        ]
                        [ text "go to newest commit" ]
                    ]
                ]
            ]
        ]
    ]


renderPendingAnalysisPane : String -> List String -> List (Html.Html Msg)
renderPendingAnalysisPane currentCommitId forCommits =
    [ div
        [ class "section has-text-centered" ]
        [ div [ class "title" ] [ text "Documentation Being Analyzed" ]
        , div [ class "subtitle" ] [ text "refresh the page in a bit..." ]
        , progress
            [ class "progress is-small is-success"
            , style "width" "50%"
            , style "margin" "auto"
            , style "margin-bottom" "20px"
            ]
            []
        , div
            [ class "content" ]
            (case forCommits of
                [ _ ] ->
                    [ text "this commit is being analyzed" ]

                _ ->
                    [ text "Commits Queued for Analysis"
                    , ol [] <|
                        List.map
                            (\commitId ->
                                li
                                    [ classList
                                        [ ( "has-text-weight-bold"
                                          , commitId == currentCommitId
                                          )
                                        ]
                                    ]
                                    [ text commitId ]
                            )
                            forCommits
                    ]
            )
        ]
    ]


renderAnalysisFailedPane : String -> List (Html.Html Msg)
renderAnalysisFailedPane withReason =
    [ div
        [ class "section has-text-centered" ]
        [ div [ class "title" ] [ text "Analysis Error" ]
        , div [ class "subtitle" ] [ text "VivaDoc was unable to analyze this commit" ]
        , hr
            [ style "width" "50%"
            , style "margin" "auto"
            , style "margin-bottom" "20px"
            ]
            []
        , p [ class "content has-text-grey" ] [ text withReason ]
        ]
    ]


renderGetCommitReviewErrorModal : Core.HttpError GcrError.GetCommitReviewError -> List (Html.Html Msg)
renderGetCommitReviewErrorModal httpError =
    let
        internalErrorText =
            "Internal Error...try to refresh the page in a bit!"

        modalText =
            case httpError of
                Core.BadUrl string ->
                    internalErrorText

                Core.Timeout ->
                    "There was a timeout trying to retrieve the commit analysis...try again in a bit."

                Core.NetworkError ->
                    "There seems to be a network error, are you connected to the internet? Refresh the page once you are."

                Core.BadSuccessBody string ->
                    internalErrorText

                Core.BadErrorBody string ->
                    internalErrorText

                Core.BadStatus int getCommitReviewErrorGcrError ->
                    case getCommitReviewErrorGcrError of
                        GcrError.UnknownError ->
                            internalErrorText
    in
    [ div
        [ class "modal is-active" ]
        [ div [ class "modal-background" ] []
        , div
            [ class "modal-card" ]
            [ section
                [ class "modal-card-head" ]
                [ Icon.renderIcon
                    { iconName = "error"
                    , optionalAdjacentText = Just ( "Error", Bulma.DarkGrey )
                    , iconSize = Bulma.BulmaMedium
                    , iconColor = Bulma.Danger
                    }
                ]
            , section
                [ class "modal-card-body" ]
                [ text modalText ]
            ]
        ]
    ]



-- UPDATE


type Msg
    = CompletedGetCommitReview (Result.Result (Core.HttpError GcrError.GetCommitReviewError) GcrResponse.CommitReviewResponse)
    | ToggleShowAlteredLines Language.Language CommitReview.Review
    | SetModalClosed Bool GcrResponse.CommitReviewResponseType
    | SetAuthorFilter CommitReview.AuthorFilter
    | SetReviewStateFilter CommitReview.ReviewStateFilter
    | AddToDocReview UA.AssessmentType String
    | RemoveFromDocReview String
    | SubmitDocReview String CommitReview.DocReviewTagIds
    | CompletedSubmitDocReview String CommitReview.DocReviewTagIds (Result.Result (Core.HttpError PuaError.PostUserAssessmentsError) PuaResponse.PostUserAssessmentsResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateCompleteCommitReview updater modelCommitReview =
            modelCommitReview |> (RemoteData.map << GcrResponse.mapComplete) updater
    in
    case msg of
        CompletedGetCommitReview (Result.Ok response) ->
            ( { model | commitReview = RemoteData.Success response }
            , case response.responseType of
                GcrResponse.Complete commitReview ->
                    if response.headCommitId == model.commitId then
                        Ports.renderCodeEditors <|
                            CommitReview.extractRenderEditorConfigs commitReview

                    else
                        Cmd.none

                _ ->
                    Cmd.none
            )

        CompletedGetCommitReview (Result.Err err) ->
            ( { model | commitReview = RemoteData.Failure err }, Cmd.none )

        SetAuthorFilter authorFilter ->
            ( { model
                | authorFilter = authorFilter
                , commitReview =
                    model.commitReview
                        |> updateCompleteCommitReview
                            (CommitReview.updateCommitReviewForFilters
                                authorFilter
                                model.reviewStateFilter
                            )
              }
            , Cmd.none
            )

        SetReviewStateFilter reviewStateFilter ->
            ( { model
                | reviewStateFilter = reviewStateFilter
                , commitReview =
                    model.commitReview
                        |> updateCompleteCommitReview
                            (CommitReview.updateCommitReviewForFilters
                                model.authorFilter
                                reviewStateFilter
                            )
              }
            , Cmd.none
            )

        ToggleShowAlteredLines language forReview ->
            let
                updatedReview =
                    { forReview
                        | reviewType =
                            case forReview.reviewType of
                                CommitReview.ReviewModifiedTag showAlteredLines ->
                                    CommitReview.ReviewModifiedTag <| not showAlteredLines

                                CommitReview.ReviewNewTag showAlteredLines ->
                                    CommitReview.ReviewNewTag <| not showAlteredLines

                                CommitReview.ReviewDeletedTag currentFileStartLineNumber ->
                                    CommitReview.ReviewDeletedTag currentFileStartLineNumber
                    }
            in
            ( { model
                | commitReview =
                    model.commitReview
                        |> updateCompleteCommitReview
                            (CommitReview.updateReviews
                                (\review ->
                                    if review.tag.tagId == updatedReview.tag.tagId then
                                        updatedReview

                                    else
                                        review
                                )
                            )
              }
            , Ports.rerenderCodeEditor <|
                CommitReview.renderConfigForReviewOrTag language (CommitReview.AReview updatedReview)
            )

        SetModalClosed modalClosed gcrResponseType ->
            ( { model | modalClosed = modalClosed }
            , case ( gcrResponseType, modalClosed ) of
                ( GcrResponse.Complete commitReview, True ) ->
                    Ports.renderCodeEditors <| CommitReview.extractRenderEditorConfigs commitReview

                _ ->
                    Cmd.none
            )

        AddToDocReview assessmentType tagId ->
            ( { model
                | commitReview =
                    model.commitReview
                        |> updateCompleteCommitReview
                            (CommitReview.updateTagApprovalState
                                { tagId = tagId, approvalState = CommitReview.InDocReview assessmentType }
                            )
                        |> updateCompleteCommitReview
                            (CommitReview.updateCommitReviewForFilters model.authorFilter model.reviewStateFilter)
              }
            , Cmd.none
            )

        RemoveFromDocReview tagId ->
            ( { model
                | commitReview =
                    model.commitReview
                        |> updateCompleteCommitReview
                            (CommitReview.updateTagApprovalState
                                { tagId = tagId, approvalState = CommitReview.Neutral }
                            )
                        |> updateCompleteCommitReview
                            (CommitReview.updateCommitReviewForFilters model.authorFilter model.reviewStateFilter)
              }
            , Cmd.none
            )

        SubmitDocReview username ({ markedForApprovalTagIds, markedForRejectionTagIds } as docReviewTagIds) ->
            ( { model
                | submitDocReviewState = Loading
                , commitReview =
                    model.commitReview
                        |> updateCompleteCommitReview
                            (CommitReview.updateApprovalStatesForTags <|
                                CommitReview.docReviewTagIdsToTagAndApprovalState
                                    docReviewTagIds
                                    CommitReview.InDocReviewBeingSubmitted
                            )
              }
            , Api.postUserAssessments
                model.repoId
                model.prNumber
                model.commitId
                markedForApprovalTagIds
                markedForRejectionTagIds
                (CompletedSubmitDocReview username docReviewTagIds)
            )

        CompletedSubmitDocReview username _ (Result.Ok response) ->
            ( { model
                | submitDocReviewState =
                    if PuaResponse.allUserAssessmentsSucceeded response then
                        NotAsked

                    else
                        PartialFailure
                , commitReview =
                    model.commitReview
                        |> updateCompleteCommitReview
                            (CommitReview.updateTags
                                (\tag ->
                                    case PuaResponse.getResponseForTagId tag.tagId response of
                                        Nothing ->
                                            tag

                                        Just { responseType } ->
                                            case responseType of
                                                PuaResponse.ApprovalSuccess isApproved ->
                                                    { tag
                                                        | userAssessments =
                                                            { tagId = tag.tagId
                                                            , username = username
                                                            , assessmentType = UA.Approved
                                                            }
                                                                :: tag.userAssessments
                                                        , approvedState =
                                                            if isApproved then
                                                                CommitReview.NonNeutral UA.Approved

                                                            else
                                                                CommitReview.Neutral
                                                    }

                                                PuaResponse.RejectionSuccess ->
                                                    { tag
                                                        | userAssessments =
                                                            { tagId = tag.tagId
                                                            , username = username
                                                            , assessmentType = UA.Rejected
                                                            }
                                                                :: tag.userAssessments
                                                        , approvedState = CommitReview.NonNeutral UA.Rejected
                                                    }

                                                PuaResponse.ApprovalFailure ->
                                                    { tag | approvedState = CommitReview.RequestFailed () }

                                                PuaResponse.RejectionFailure ->
                                                    { tag | approvedState = CommitReview.RequestFailed () }
                                )
                            )
                        |> updateCompleteCommitReview
                            (CommitReview.updateCommitReviewForFilters model.authorFilter model.reviewStateFilter)
              }
            , Cmd.none
            )

        CompletedSubmitDocReview _ docReviewTagIds (Result.Err err) ->
            let
                neutralTagAndApprovalStates =
                    CommitReview.docReviewTagIdsToTagAndApprovalState
                        docReviewTagIds
                        (always CommitReview.Neutral)

                updateCommitReviewDocTagIdsToNeutral =
                    CommitReview.updateApprovalStatesForTags neutralTagAndApprovalStates
            in
            ( case err of
                Core.BadStatus _ (PuaError.StaleCommitError newHeadCommitId) ->
                    { model
                        | submitDocReviewState = NotAsked
                        , commitReview =
                            model.commitReview
                                |> updateCompleteCommitReview updateCommitReviewDocTagIdsToNeutral
                                |> updateCompleteCommitReview
                                    (CommitReview.updateCommitReviewForFilters
                                        model.authorFilter
                                        model.reviewStateFilter
                                    )
                                |> RemoteData.map (\crr -> { crr | headCommitId = newHeadCommitId })
                    }

                _ ->
                    { model
                        | submitDocReviewState = FullFailure err
                        , commitReview =
                            model.commitReview |> updateCompleteCommitReview updateCommitReviewDocTagIdsToNeutral
                    }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession =
    .session
