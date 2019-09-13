port module Ports exposing (RenderCodeEditorConfig, loadFromLocalStorage, onLoadFromLocalStorage, renderCodeEditors, rerenderCodeEditor, saveToLocalStorage)

import Json.Encode as Encode


port saveToLocalStorage : Encode.Value -> Cmd msg


port loadFromLocalStorage : () -> Cmd msg


port onLoadFromLocalStorage : (String -> msg) -> Sub msg


type alias RenderCodeEditorConfig =
    { tagId : String
    , startLineNumber : Int
    , customLineNumbers : Maybe (List (Maybe Int))
    , redLineRanges : List ( Int, Int )
    , greenLineRanges : List ( Int, Int )
    , content : List String
    , language : String
    }


port renderCodeEditors : List RenderCodeEditorConfig -> Cmd msg


port rerenderCodeEditor : RenderCodeEditorConfig -> Cmd msg
