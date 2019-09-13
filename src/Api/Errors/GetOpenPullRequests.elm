module Api.Errors.GetOpenPullRequests exposing (GetOpenPullRequestsError(..), decodeGetOpenPullRequestsError)

import Json.Decode as Decode


type GetOpenPullRequestsError
    = UnknownError


decodeGetOpenPullRequestsError : Decode.Decoder GetOpenPullRequestsError
decodeGetOpenPullRequestsError =
    Decode.succeed UnknownError
