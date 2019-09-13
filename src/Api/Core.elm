module Api.Core exposing (FormError(..), HttpError(..), Repo, Repos, Username, delete, expectJson, expectJsonWithUserAndRepos, get, getFormErrors, getInstalledRepos, getRepoAppInstalledStatus, getRepoFullName, getRepoId, getRepoNameAndOwner, getRepoPrivateStatus, getRepos, getUsername, post, put)

{-| This module provides a few core API-related responsibilities:

  - Providing the private User/Repos opaque types which you can only get from an HttpRequest.
  - Providing HTTP-request helpers which use `Endpoint` and `HttpError`
  - Providing a modified `Http.Error` and `FormError` types
  - Providing helpers for dealing with form errors

This module does NOT contain the actual routes to the API though, refer to the `Api.Api` module for
interaction with the API.

-}

import Api.Endpoint as Endpoint exposing (Endpoint)
import Browser
import Browser.Navigation as Nav
import Http
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string)
import Json.Decode.Pipeline as Pipeline exposing (required)
import Json.Encode as Encode
import Url exposing (Url)



-- TYPES


{-| Keep this private so the only way to create this is on an HttpRequest.
-}
type Username
    = Username String


{-| Keep this private so the only way to create this is on an HttpRequest.
-}
type Repos
    = Repos (List Repo)


{-| Keep this private so the only way to create this is on an HttpRequest.
-}
type Repo
    = Repo Int String Bool Bool -- id, full_name, is_private, appInstalled


getUsername : Username -> String
getUsername (Username username) =
    username


getRepos : Repos -> List Repo
getRepos (Repos repos) =
    repos


getInstalledRepos : List Repo -> List Repo
getInstalledRepos =
    List.filter getRepoAppInstalledStatus


getRepoFullName : Repo -> String
getRepoFullName (Repo _ fullName _ _) =
    fullName


getRepoNameAndOwner : Repo -> { name : String, owner : String }
getRepoNameAndOwner (Repo _ fullName _ _) =
    String.split "/" fullName
        |> (\splitStr ->
                case splitStr of
                    [ owner, name ] ->
                        { owner = owner, name = name }

                    _ ->
                        { owner = "", name = "" }
           )


getRepoId : Repo -> Int
getRepoId (Repo repoId _ _ _) =
    repoId


getRepoPrivateStatus : Repo -> Bool
getRepoPrivateStatus (Repo _ _ isPrivate _) =
    isPrivate


getRepoAppInstalledStatus : Repo -> Bool
getRepoAppInstalledStatus (Repo _ _ _ appInstalled) =
    appInstalled


{-| Keep this private so the only way to get `Username` and `Repos` is through the http request.
-}
decodeUsernameReposAnd : Decode.Decoder (Username -> Repos -> a) -> Decode.Decoder a
decodeUsernameReposAnd decoder =
    let
        decodeUsername =
            Decode.field "username" Decode.string
                |> Decode.map Username

        decodeRepos =
            Decode.field "repos" (Decode.list decodeRepo)
                |> Decode.map Repos
    in
    Decode.map3 (\fromUsernameAndRepos username repos -> fromUsernameAndRepos username repos)
        decoder
        decodeUsername
        decodeRepos


decodeRepo : Decode.Decoder Repo
decodeRepo =
    Decode.map4 Repo
        (Decode.field "id" Decode.int)
        (Decode.field "full_name" Decode.string)
        (Decode.field "private" Decode.bool)
        (Decode.field "appInstalled" Decode.bool)



-- APPLICATION
-- HTTP HELPERS


{-| All possible HTTP errors, similar to `Http.Error` but `Http.BasStatus` will include the response body making
it much more practical for displaying errors from the server.

NOTE: Realistically you would never display the BadSuccessBody/BadErrorBody decode error string to the user but
I am still keeping it because it'll be useful during development. In production you can just ignore that string
and display the error you were going to display regardless.

-}
type HttpError errorBody
    = BadUrl String
    | Timeout
    | NetworkError
    | BadSuccessBody String -- String is the decode failure explanatory string
    | BadErrorBody String -- String is the decode failure explanatory string
    | BadStatus Int errorBody



-- FORM ERROR HELPERS


{-| A form can have errors or get errors prior to the http request in the client or after from the server.
-}
type FormError serverError clientError
    = NoError
    | HttpError (HttpError serverError)
    | ClientError clientError


{-| Get all errors which apply to the entire form.

Ignore field-specific errors in the map functions.

-}
getFormErrors : FormError serverError clientError -> (serverError -> List String) -> (clientError -> List String) -> List String
getFormErrors fe serverErrorToList clientErrorToList =
    let
        internalError =
            [ "sorry, there is an internal error right now" ]
    in
    case fe of
        NoError ->
            []

        ClientError clientError ->
            clientErrorToList clientError

        HttpError (BadUrl _) ->
            internalError

        HttpError Timeout ->
            [ "sorry, the request timed out" ]

        HttpError NetworkError ->
            [ "there appears to be a network connection issue" ]

        HttpError (BadSuccessBody _) ->
            internalError

        HttpError (BadErrorBody _) ->
            internalError

        HttpError (BadStatus _ serverError) ->
            serverErrorToList serverError


{-| Similar to `Http.expectJson` but this uses our custom `HttpError` which has a body on the server error response
instead of just a status code.
-}
expectJson : (Result (HttpError errorBody) successBody -> msg) -> Decode.Decoder successBody -> Decode.Decoder errorBody -> Http.Expect msg
expectJson toMsg successDecoder errorDecoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ metadata body ->
                    case Decode.decodeString errorDecoder body of
                        Ok value ->
                            Err <| BadStatus metadata.statusCode value

                        Err err ->
                            Err <| BadErrorBody <| Decode.errorToString err

                Http.GoodStatus_ metadata body ->
                    case Decode.decodeString successDecoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err <| BadSuccessBody <| Decode.errorToString err


{-| Similar to `expectJson` above but expects a username and repos in the response, otherwise
you can't decode that information as it is opaque to this module.
-}
expectJsonWithUserAndRepos : (Result (HttpError errorBody) successBody -> msg) -> Decode.Decoder (Username -> Repos -> successBody) -> Decode.Decoder errorBody -> Http.Expect msg
expectJsonWithUserAndRepos toMsg successDecoder errorDecoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ metadata body ->
                    case Decode.decodeString errorDecoder body of
                        Ok value ->
                            Err <| BadStatus metadata.statusCode value

                        Err err ->
                            Err <| BadErrorBody <| Decode.errorToString err

                Http.GoodStatus_ metadata body ->
                    case Decode.decodeString (decodeUsernameReposAnd successDecoder) body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err <| BadSuccessBody <| Decode.errorToString err



-- HTTP METHODS


get : Endpoint -> Maybe Float -> Maybe String -> Http.Expect a -> Cmd.Cmd a
get url timeout tracker expect =
    Endpoint.request
        { method = "GET"
        , url = url
        , expect = expect
        , headers = []
        , body = Http.emptyBody
        , timeout = timeout
        , tracker = tracker
        }


put : Endpoint -> Maybe Float -> Maybe String -> Http.Body -> Http.Expect a -> Cmd.Cmd a
put url timeout tracker body expect =
    Endpoint.request
        { method = "PUT"
        , url = url
        , expect = expect
        , headers = []
        , body = body
        , timeout = timeout
        , tracker = tracker
        }


post : Endpoint -> Maybe Float -> Maybe String -> Http.Body -> Http.Expect a -> Cmd.Cmd a
post url timeout tracker body expect =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = expect
        , headers = []
        , body = body
        , timeout = timeout
        , tracker = tracker
        }


delete : Endpoint -> Maybe Float -> Maybe String -> Http.Body -> Http.Expect a -> Cmd.Cmd a
delete url timeout tracker body expect =
    Endpoint.request
        { method = "DELETE"
        , url = url
        , expect = expect
        , headers = []
        , body = body
        , timeout = timeout
        , tracker = tracker
        }
