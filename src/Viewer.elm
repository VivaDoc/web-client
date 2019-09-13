module Viewer exposing (Viewer, decodeViewer, getRepos, getUsername)

{-| The logged-in user currently viewing this page.

It stores the username along with their repos.

NOTE: Authentication is currently based on a cookie so it's possible that the cookie
expires while they are logged in.

-}

import Api.Core as Core
import Json.Decode as Decode



-- TYPES


type Viewer
    = Viewer Core.Username Core.Repos


decodeViewer : Decode.Decoder (Core.Username -> Core.Repos -> Viewer)
decodeViewer =
    Decode.succeed Viewer



-- INFO


getUsername : Viewer -> String
getUsername (Viewer username _) =
    Core.getUsername username


getRepos : Viewer -> List Core.Repo
getRepos (Viewer _ repos) =
    Core.getRepos repos
