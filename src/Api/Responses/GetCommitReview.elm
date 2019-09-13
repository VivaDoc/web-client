module Api.Responses.GetCommitReview exposing (CommitReviewResponse, CommitReviewResponseType(..), decodeCommitReviewResponse, mapComplete)

import CommitReview
import Json.Decode as Decode


type alias CommitReviewResponse =
    { headCommitId : String
    , responseType : CommitReviewResponseType
    }


type CommitReviewResponseType
    = Pending (List String)
    | Complete CommitReview.CommitReview
    | AnalysisFailed String


mapComplete : (CommitReview.CommitReview -> CommitReview.CommitReview) -> CommitReviewResponse -> CommitReviewResponse
mapComplete updater crr =
    { crr
        | responseType =
            case crr.responseType of
                Complete commitReview ->
                    Complete <| updater commitReview

                _ ->
                    crr.responseType
    }


decodeCommitReviewResponse : Decode.Decoder CommitReviewResponse
decodeCommitReviewResponse =
    Decode.map2 CommitReviewResponse
        (Decode.field "headCommitId" Decode.string)
        decodeResponseType


decodeResponseType : Decode.Decoder CommitReviewResponseType
decodeResponseType =
    Decode.field "responseTag" Decode.string
        |> Decode.andThen
            (\responseTag ->
                case responseTag of
                    "pending" ->
                        Decode.map Pending <|
                            Decode.field "forCommits" (Decode.list Decode.string)

                    "complete" ->
                        Decode.map Complete <|
                            Decode.field "commitReview" CommitReview.decodeCommitReview

                    "analysis-failed" ->
                        Decode.map AnalysisFailed <|
                            Decode.field "clientExplanation" Decode.string

                    _ ->
                        Decode.fail <| "Response tag not valid: " ++ responseTag
            )
