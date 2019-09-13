module Api.Errors.GetCommitReview exposing (GetCommitReviewError(..), decodeGetCommitReviewError)

import Json.Decode as Decode


type GetCommitReviewError
    = UnknownError


decodeGetCommitReviewError : Decode.Decoder GetCommitReviewError
decodeGetCommitReviewError =
    Decode.succeed UnknownError
