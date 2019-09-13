module Api.Responses.PostUserAssessments exposing (PostUserAssessmentsResponse, UserAssessmentResponse, UserAssessmentResponseType(..), allUserAssessmentsSucceeded, decodePostUserAssessmentsResponse, decodeUserAssessmentResponse, getResponseForTagId)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Set


type alias PostUserAssessmentsResponse =
    List UserAssessmentResponse


type alias UserAssessmentResponse =
    { tagId : String
    , responseType : UserAssessmentResponseType
    }


type UserAssessmentResponseType
    = ApprovalSuccess Bool
    | RejectionSuccess
    | ApprovalFailure
    | RejectionFailure


getResponseForTagId : String -> PostUserAssessmentsResponse -> Maybe UserAssessmentResponse
getResponseForTagId tagId =
    List.filter (userAssessmentResponseIsForTagId tagId) >> List.head


userAssessmentResponseIsForTagId : String -> UserAssessmentResponse -> Bool
userAssessmentResponseIsForTagId tagId =
    .tagId >> (==) tagId


allUserAssessmentsSucceeded : PostUserAssessmentsResponse -> Bool
allUserAssessmentsSucceeded =
    List.all userAssessmentIsSuccess


userAssessmentIsSuccess : UserAssessmentResponse -> Bool
userAssessmentIsSuccess userAssessment =
    case userAssessment.responseType of
        ApprovalSuccess _ ->
            True

        RejectionSuccess ->
            True

        _ ->
            False


decodePostUserAssessmentsResponse : Decode.Decoder PostUserAssessmentsResponse
decodePostUserAssessmentsResponse =
    Decode.list decodeUserAssessmentResponse


decodeUserAssessmentResponse : Decode.Decoder UserAssessmentResponse
decodeUserAssessmentResponse =
    Decode.map2 UserAssessmentResponse
        (Decode.field "tagId" Decode.string)
        (Decode.field "status" Decode.string
            |> Decode.andThen
                (\status ->
                    case status of
                        "approval-success" ->
                            Decode.succeed ApprovalSuccess
                                |> required "tagApproved" Decode.bool

                        "rejection-success" ->
                            Decode.succeed <| RejectionSuccess

                        "approval-failure" ->
                            Decode.succeed <| ApprovalFailure

                        "rejection-failure" ->
                            Decode.succeed <| RejectionFailure

                        _ ->
                            Decode.fail <| status ++ " is not a valid status."
                )
        )
