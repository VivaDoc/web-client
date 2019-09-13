module CodeEditor exposing (codeEditor)

import Html exposing (pre)
import Html.Attributes exposing (id)


{-| Get the markdown for some content.
-}
codeEditor : String -> Html.Html msg
codeEditor tagId =
    pre [ id <| "editor-" ++ tagId ] []
