module Page.Redirect exposing (RedirectDisplay(..), view)

import Html exposing (div)
import Loading


type RedirectDisplay
    = FetchingUser
    | Blank


view : RedirectDisplay -> { title : String, content : Html.Html msg }
view redirectReason =
    case redirectReason of
        FetchingUser ->
            { title = "Fetching User"
            , content = Loading.renderLoadingScreen "fetching user"
            }

        Blank ->
            { title = ""
            , content = div [] []
            }
