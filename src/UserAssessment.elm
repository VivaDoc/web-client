module UserAssessment exposing (AssessmentType(..), UserAssessment, decodeUserAssessment, hasTagId, isAll, isApproved, isForUser, isRejected, prettyPrintAssessmentType, prettyPrintAssessmentTypeWithCapital)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (hardcoded, optional, required)


type alias UserAssessment =
    { tagId : String
    , assessmentType : AssessmentType
    , username : String
    }


type AssessmentType
    = Approved
    | Rejected


isAll : List (UserAssessment -> Bool) -> UserAssessment -> Bool
isAll hasProperties userAssessment =
    List.all (\hasProperty -> hasProperty userAssessment) hasProperties


isApproved : AssessmentType -> Bool
isApproved assessmentType =
    case assessmentType of
        Approved ->
            True

        Rejected ->
            False


isRejected : AssessmentType -> Bool
isRejected assessmentType =
    case assessmentType of
        Approved ->
            False

        Rejected ->
            True


hasTagId : String -> UserAssessment -> Bool
hasTagId tagId userAssessment =
    tagId == userAssessment.tagId


isForUser : String -> UserAssessment -> Bool
isForUser username userAssessment =
    username == userAssessment.username


prettyPrintAssessmentTypeWithCapital : AssessmentType -> String
prettyPrintAssessmentTypeWithCapital assessmentType =
    case assessmentType of
        Approved ->
            "Approved"

        Rejected ->
            "Rejected"


prettyPrintAssessmentType : AssessmentType -> String
prettyPrintAssessmentType =
    prettyPrintAssessmentTypeWithCapital >> String.toLower


decodeUserAssessment : Decode.Decoder UserAssessment
decodeUserAssessment =
    Decode.succeed UserAssessment
        |> required "tagId" Decode.string
        |> required "assessmentType" decodeAssessmentType
        |> required "username" Decode.string


decodeAssessmentType : Decode.Decoder AssessmentType
decodeAssessmentType =
    Decode.string
        |> Decode.andThen
            (\assessmentTypeAsString ->
                case assessmentTypeAsString of
                    "approved" ->
                        Decode.succeed Approved

                    "rejected" ->
                        Decode.succeed Rejected

                    _ ->
                        Decode.fail <| assessmentTypeAsString ++ " is not a valid assessment type."
            )
