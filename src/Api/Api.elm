module Api.Api exposing (GithubLoginBody, getCommitReview, getLogout, getOpenPullRequests, getUser, githubLoginFromCode, postUserAssessments)

{-| This module strictly contains the routes to the API.

NOTE: Extra things that are unrelated to the API requests and handling their errors should most
likely be put in `Api.Core`.

-}

import Api.Core as Core
import Api.Endpoint as Endpoint
import Api.Errors.GetCommitReview as GcrError
import Api.Errors.GetOpenPullRequests as GoprError
import Api.Errors.PostUserAssessments as PuaError
import Api.Responses.GetCommitReview as GcrResponse
import Api.Responses.PostUserAssessments as PuaResponse
import CommitReview
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import PullRequest
import Set
import Viewer


type alias GithubLoginBody =
    { code : String }


standardTimeout =
    Just (seconds 10)


longTimeout =
    Just (seconds 10)


{-| TODO handle errors
-}
githubLoginFromCode : GithubLoginBody -> (Result.Result (Core.HttpError ()) Viewer.Viewer -> msg) -> Cmd.Cmd msg
githubLoginFromCode { code } handleResult =
    Core.get
        (Endpoint.githubLoginFromCode code)
        standardTimeout
        Nothing
        (Core.expectJsonWithUserAndRepos handleResult Viewer.decodeViewer (Decode.succeed ()))


{-| TODO handle errors.
-}
getUser : (Result.Result (Core.HttpError ()) Viewer.Viewer -> msg) -> Cmd.Cmd msg
getUser handleResult =
    Core.get
        Endpoint.user
        standardTimeout
        Nothing
        (Core.expectJsonWithUserAndRepos handleResult Viewer.decodeViewer (Decode.succeed ()))


{-| TODO handle errors.
-}
getCommitReview :
    Int
    -> Int
    -> String
    -> (Result.Result (Core.HttpError GcrError.GetCommitReviewError) GcrResponse.CommitReviewResponse -> msg)
    -> Cmd.Cmd msg
getCommitReview repoId prNumber commitId handleResult =
    Core.get
        (Endpoint.commitReview repoId prNumber commitId)
        standardTimeout
        Nothing
        (Core.expectJson handleResult GcrResponse.decodeCommitReviewResponse GcrError.decodeGetCommitReviewError)


getOpenPullRequests :
    Int
    -> (Result.Result (Core.HttpError GoprError.GetOpenPullRequestsError) (List PullRequest.PullRequest) -> msg)
    -> Cmd.Cmd msg
getOpenPullRequests repoId handleResult =
    Core.get
        (Endpoint.openPullRequests repoId)
        standardTimeout
        Nothing
        (Core.expectJson handleResult PullRequest.decodePullRequests GoprError.decodeGetOpenPullRequestsError)


{-| TODO handle errors.
-}
postUserAssessments :
    Int
    -> Int
    -> String
    -> Set.Set String
    -> Set.Set String
    -> (Result.Result (Core.HttpError PuaError.PostUserAssessmentsError) PuaResponse.PostUserAssessmentsResponse -> msg)
    -> Cmd.Cmd msg
postUserAssessments repoId prNumber commitId approveTags rejectTags handleResult =
    let
        encodedBody =
            Encode.object
                [ ( "approveTags", Encode.set Encode.string approveTags )
                , ( "rejectTags", Encode.set Encode.string rejectTags )
                ]
    in
    Core.post
        (Endpoint.commitReviewUserAssessments repoId prNumber commitId)
        standardTimeout
        Nothing
        (Http.jsonBody encodedBody)
        (Core.expectJson handleResult PuaResponse.decodePostUserAssessmentsResponse PuaError.decodePostUserAssessmentsError)


{-| TODO care about the results beyond success/error (aka unit types).
-}
getLogout : (Result.Result (Core.HttpError ()) () -> msg) -> Cmd.Cmd msg
getLogout handleResult =
    Core.get
        Endpoint.logout
        (Just (seconds 10))
        Nothing
        (Core.expectJson handleResult (Decode.succeed ()) (Decode.succeed ()))



-- INTERNAL HELPERS


{-| Convert seconds to milliseconds.
-}
seconds : Float -> Float
seconds =
    (*) 1000


{-| Decode a single string error into a list with 1 string error.
-}
decodeFieldError : Decode.Decoder (List String)
decodeFieldError =
    Decode.string
        |> Decode.map (\err -> [ err ])


{-| Decode a list of string errors.
-}
decodeFieldErrors : Decode.Decoder (List String)
decodeFieldErrors =
    Decode.list Decode.string
