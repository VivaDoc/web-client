module Session exposing (Session(..), fromViewer, getNavKey, getViewer)

{-| A session contains a `Nav.Key` and a `Viewer.Viewer` if you are logged-in.
-}

import Browser.Navigation as Nav
import Viewer exposing (Viewer)



-- TYPES


type Session
    = LoggedIn Nav.Key Viewer
    | Guest Nav.Key



-- INFO


getViewer : Session -> Maybe Viewer
getViewer session =
    case session of
        LoggedIn _ val ->
            Just val

        Guest _ ->
            Nothing


getNavKey : Session -> Nav.Key
getNavKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key



-- CHANGES


fromViewer : Nav.Key -> Maybe Viewer -> Session
fromViewer key maybeViewer =
    case maybeViewer of
        Just viewerVal ->
            LoggedIn key viewerVal

        Nothing ->
            Guest key
