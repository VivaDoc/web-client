module LocalStorage exposing (LocalStorage, clearModel, decodeLocalStorage, encodeLocalStorage, loadModel, saveModel)

import Json.Decode as Decode
import Json.Encode as Encode
import Ports


{-| Currently we just temporarily save the branch page review info for redirecting back after successful github oauth.
-}
type alias LocalStorage =
    { relativeUrl : String
    }


saveModel : LocalStorage -> Cmd msg
saveModel =
    encodeLocalStorage >> Ports.saveToLocalStorage


loadModel : () -> Cmd msg
loadModel =
    Ports.loadFromLocalStorage


clearModel : Cmd msg
clearModel =
    Ports.saveToLocalStorage <| Encode.null


encodeLocalStorage : LocalStorage -> Encode.Value
encodeLocalStorage localStorage =
    Encode.object
        [ ( "relativeUrl", Encode.string localStorage.relativeUrl ) ]


decodeLocalStorage : Decode.Decoder LocalStorage
decodeLocalStorage =
    Decode.map LocalStorage
        (Decode.field "relativeUrl" Decode.string)
