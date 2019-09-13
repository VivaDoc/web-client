module Page.Repo exposing (Model, Msg(..), init, update, view)

import Api.Api as Api
import Api.Core as Core
import Api.Errors.GetOpenPullRequests as GoprError
import FetchData
import Github
import Html exposing (a, button, div, h1, h3, section, span, text)
import Html.Attributes exposing (class, style)
import Loading
import PullRequest
import Route
import Session
import Words


type alias Model =
    { session : Session.Session
    , repoId : Int
    , openPullRequests : FetchData.FetchData (Core.HttpError GoprError.GetOpenPullRequestsError) (List PullRequest.PullRequest)
    }


init : Session.Session -> Int -> ( Model, Cmd.Cmd Msg )
init session repoId =
    ( { session = session
      , repoId = repoId
      , openPullRequests = FetchData.Loading
      }
    , Api.getOpenPullRequests repoId CompletedGetOpenPullRequests
    )


view : Model -> { title : String, content : Html.Html msg }
view model =
    { title = "Repo"
    , content =
        case model.session of
            Session.Guest _ ->
                div [] [ text "You must be logged in to view this page." ]

            Session.LoggedIn _ viewer ->
                case model.openPullRequests of
                    FetchData.Loading ->
                        Loading.renderLoadingScreen "loading pull requests"

                    FetchData.Success pullRequests ->
                        renderPullRequests model.repoId pullRequests

                    FetchData.Failure err ->
                        renderErrorScreen err
    }


renderErrorScreen : Core.HttpError GoprError.GetOpenPullRequestsError -> Html.Html msg
renderErrorScreen err =
    div
        [ class "section is-large has-text-centered" ]
        [ div
            [ class "title is-2" ]
            [ text "Internal Error"
            , div [ class "subtitle is-5" ] [ text "try again later..." ]
            ]
        ]


renderPullRequests : Int -> List PullRequest.PullRequest -> Html.Html msg
renderPullRequests repoId pullRequests =
    section
        [ class "section" ]
        [ h1
            [ class "title is-4 has-text-centered" ]
            [ text "Open Pull Requests" ]
        , if List.isEmpty pullRequests then
            div
                [ class "section is-small has-text-centered" ]
                [ text "this repository has no open pull requests" ]

          else
            div
                [ class "columns is-multiline"
                , style "margin" "0px"
                ]
                (List.map (renderPullRequest repoId) pullRequests)
        ]


renderPullRequest : Int -> PullRequest.PullRequest -> Html.Html msg
renderPullRequest repoId pullRequest =
    div
        [ class "column is-6" ]
        [ div
            [ class "box has-text-centered"
            , style "height" "200px"
            , style "padding" "10px"
            ]
            [ div
                [ class "level is-mobile"
                , style "width" "100%"
                , style "height" "20px"
                , style "margin-bottom" "35px"
                ]
                [ div
                    [ class "level-item level-left" ]
                    [ Github.githubIcon pullRequest.htmlUrl ]
                , div
                    [ class "level-item level-right has-text-grey-light" ]
                    [ text <| "#" ++ String.fromInt pullRequest.number ]
                ]
            , div
                [ class "level"
                , style "height" "70px"
                ]
                [ div
                    [ class "level-item has-text-weight-medium"
                    , style "height" "70px"
                    , style "width" "100%"
                    , style "padding" "0 10px"
                    ]
                    [ a
                        [ Route.href <| Route.CommitReview repoId pullRequest.number pullRequest.headCommitId ]
                        [ text <| Words.ellipsify 150 pullRequest.title ]
                    ]
                ]
            ]
        ]


type Msg
    = CompletedGetOpenPullRequests (Result.Result (Core.HttpError GoprError.GetOpenPullRequestsError) (List PullRequest.PullRequest))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompletedGetOpenPullRequests (Result.Ok pullRequests) ->
            ( { model | openPullRequests = FetchData.Success pullRequests }, Cmd.none )

        CompletedGetOpenPullRequests (Result.Err err) ->
            ( { model | openPullRequests = FetchData.Failure err }, Cmd.none )
