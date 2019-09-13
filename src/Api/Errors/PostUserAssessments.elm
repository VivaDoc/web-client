module Api.Errors.PostUserAssessments exposing (PostUserAssessmentsError(..), decodePostUserAssessmentsError)

import Json.Decode as Decode


type PostUserAssessmentsError
    = UnknownError
    | StaleCommitError String


decodePostUserAssessmentsError : Decode.Decoder PostUserAssessmentsError
decodePostUserAssessmentsError =
    Decode.oneOf
        [ decodeCommitStaleError |> Decode.map StaleCommitError
        , Decode.succeed UnknownError
        ]


decodeCommitStaleError : Decode.Decoder String
decodeCommitStaleError =
    Decode.field "errorCode" Decode.int
        |> Decode.andThen
            (\errorCode ->
                if errorCode == 7 then
                    Decode.field "data" Decode.string

                else
                    Decode.fail "wrong error code"
            )
